import org.mozilla.javascript.{Node, Token}
import org.mozilla.javascript.ast._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object DestructuringAssignmentsExtractor {

  private object IntElementGet {
    def unapply(node: Node): Option[(AstNode, Int)] =
      node match {
        case elementGet: ElementGet =>
          elementGet.getElement match {
            case numberLiteral: NumberLiteral =>
              tryToInt(numberLiteral.getValue) match {
                case Some(index) => Some((elementGet.getTarget, index))
                case None => None
              }
            case _ => None
          }
        case _ => None
      }

    private def tryToInt(s: String) = Try(s.toInt).toOption
  }

  def apply(astRoot: AstRoot): Unit = {
    astRoot.visit(node => {
      if (node.isInstanceOf[AstRoot] || node.isInstanceOf[Block])
        visitExtractable(node)
      true
    })
  }

  private def nameNode(identifier: String) = {
    val name = new Name
    name.setIdentifier(identifier)
    name
  }

  private def arrayDeclarationNodes(
    statementConstructor: StatementConstructor,
    declarationsFromIndices: IndexedDeclarationTargets,
    initializer: AstNode
  ): ArrayBuffer[AstNode] = {
    def arrayLiteralWithOtherDeclarations(
      declarationsFromIndices: IndexedDeclarationTargets
    ): (ArrayLiteral, ArrayBuffer[AstNode]) = {
      val arrayLiteral = new ArrayLiteral
      val variableDeclarationNodes = new mutable.ArrayBuffer[AstNode]()
      for (declarationsFromIndex <- declarationsFromIndices) {
        if (declarationsFromIndex == null)
          arrayLiteral.addElement(new EmptyExpression())
        else {
          if (declarationsFromIndex.names.isEmpty) {
            val (element, otherDeclarations) = arrayLiteralWithOtherDeclarations(declarationsFromIndex.indexed)
            arrayLiteral.addElement(element)
            variableDeclarationNodes ++= otherDeclarations
          } else {
            arrayLiteral.addElement(nameNode(declarationsFromIndex.names(0)))
            for (alias <- declarationsFromIndex.names.tail) {
              variableDeclarationNodes
                .append(statementConstructor(nameNode(alias), nameNode(declarationsFromIndex.names(0))))
            }
            if (declarationsFromIndex.indexed.nonEmpty)
              variableDeclarationNodes.appendAll(nodes(
                declarationsFromIndex.indexed,
                nameNode(declarationsFromIndex.names(0)),
              ))
          }
        }
      }
      (arrayLiteral, variableDeclarationNodes)
    }

    def nodes(indexedDeclarationTargets: IndexedDeclarationTargets, initializer: AstNode) = {
      val (target, variableDeclarations) = arrayLiteralWithOtherDeclarations(declarationsFromIndices)
      variableDeclarations.prepend(statementConstructor(target, initializer))
      variableDeclarations
    }

    nodes(declarationsFromIndices, initializer)
  }

  type IndexedDeclarationTargets = mutable.ArrayBuffer[DeclarationTargets]

  sealed trait StatementConstructor {
    def apply(target: AstNode, initializer: AstNode): AstNode
  }

  case object VarStatementConstructor extends StatementConstructor {
    override def apply(target: AstNode, initializer: AstNode): AstNode = {
      val variableDeclaration = new VariableDeclaration()
      val variableInitializer = new VariableInitializer()
      variableInitializer.setInitializer(initializer)
      variableInitializer.setTarget(target)
      variableDeclaration.addVariable(variableInitializer)
      variableDeclaration.setIsStatement(true)
      variableDeclaration
    }
  }

  case object AssignmentStatementConstructor extends StatementConstructor {
    override def apply(target: AstNode, initializer: AstNode): AstNode = {
      val expressionStatement = new ExpressionStatement()
      val assignment = new Assignment(target, initializer)
      assignment.setOperator(Token.ASSIGN)
      expressionStatement.setExpression(assignment)
      expressionStatement
    }
  }

  case class Delayed(statementType: StatementConstructor, initializerName: String)

  case class DeclarationTargets(names: mutable.ArrayBuffer[String], indexed: IndexedDeclarationTargets)

  private def visitExtractable(blockNode: Node): Unit = {
    val varDeclarationsFromArrayVariables = new mutable.LinkedHashMap[Delayed, IndexedDeclarationTargets]

    def indexDeclarationTargets(indexedDeclarationTargets: IndexedDeclarationTargets, index: Int) = {
      while (indexedDeclarationTargets.size <= index)
        indexedDeclarationTargets.append(null)
      if (indexedDeclarationTargets(index) == null)
        indexedDeclarationTargets(index) =
          DeclarationTargets(mutable.ArrayBuffer(), mutable.ArrayBuffer())
      indexedDeclarationTargets(index)
    }

    def buildVariableDeclarations() = {
      varDeclarationsFromArrayVariables.flatMap({ case (identifier, declarationsFromIndices) =>
        arrayDeclarationNodes(identifier.statementType, declarationsFromIndices, nameNode(identifier.initializerName))
      }).toList
    }

    def finalizeDeclarations(afterNode: Node) {
      buildVariableDeclarations().foreach(blockNode.addChildBefore(_, afterNode))
      varDeclarationsFromArrayVariables.clear()
    }

    def visitAssignment(node: Node, statementConstructor: StatementConstructor, left: AstNode, right: AstNode) {
      left match {
        case targetName: Name =>
          def addToIndexed(indexedDeclarationTargets: IndexedDeclarationTargets, indices: List[Int]): Boolean = {
            var indexed = indexedDeclarationTargets
            var declarationTargets: DeclarationTargets = null
            for (index <- indices) {
              declarationTargets = indexDeclarationTargets(indexed, index)
              indexed = declarationTargets.indexed
            }
            if (declarationTargets == null)
              return false
            declarationTargets.names.append(targetName.getIdentifier)
            true
          }

          def visitInitializer(indices: List[Int], initializer: AstNode): Boolean = {
            initializer match {
              case IntElementGet(target, index) => visitInitializer(index :: indices, target)
              case name: Name =>
                addToIndexed(
                  varDeclarationsFromArrayVariables.getOrElseUpdate(
                    Delayed(statementConstructor, name.getIdentifier),
                    new mutable.ArrayBuffer(),
                  ),
                  indices,
                )
              case _ =>
                finalizeDeclarations(node)
                val indexedDeclarationTargets = new IndexedDeclarationTargets
                if (addToIndexed(indexedDeclarationTargets, indices)) {
                  arrayDeclarationNodes(statementConstructor, indexedDeclarationTargets, initializer)
                    .foreach(blockNode.addChildBefore(_, node))
                  true
                } else
                  false
            }
          }

          if (visitInitializer(List.empty, right))
            blockNode.removeChild(node)
        case _ => finalizeDeclarations(node)
      }
    }

    blockNode.forEach({
      case variableDeclaration: VariableDeclaration
        if variableDeclaration.isVar && variableDeclaration.getVariables.size() == 1 =>
        val variableInitializer = variableDeclaration.getVariables.get(0)
        visitAssignment(
          variableDeclaration,
          VarStatementConstructor,
          variableInitializer.getTarget,
          variableInitializer.getInitializer,
        )
      case expressionStatement: ExpressionStatement =>
        expressionStatement.getExpression match {
          case assignment: Assignment =>
            visitAssignment(
              expressionStatement,
              AssignmentStatementConstructor,
              assignment.getLeft,
              assignment.getRight,
            )
          case _ => finalizeDeclarations(expressionStatement)
        }
      case child => finalizeDeclarations(child)
    })
    buildVariableDeclarations().foreach(blockNode.addChildToBack)
    varDeclarationsFromArrayVariables.clear()
  }
}
