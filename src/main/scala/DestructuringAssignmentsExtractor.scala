import org.mozilla.javascript.{Node, Token}
import org.mozilla.javascript.ast._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object DestructuringAssignmentsExtractor {
  private def tryToInt(s: String) = Try(s.toInt).toOption

  def apply(astRoot: AstRoot): Unit = {
    astRoot.visit(node => {
      if (node.isInstanceOf[AstRoot] || node.isInstanceOf[Block])
        visitBlock(node)
      true
    })
  }

  private def objectPropertyNode(left: AstNode, right: AstNode) = {
    val property = new ObjectProperty()
    property.setLeft(left)
    property.setRight(right)
    property
  }

  private def numberLiteralNode(value: String) = {
    val numberLiteral = new NumberLiteral
    numberLiteral.setValue(value)
    numberLiteral
  }

  private def nameNode(identifier: String) = {
    val name = new Name
    name.setIdentifier(identifier)
    name
  }

  private def stringLiteralNode(value: String, quoteChar: Char) = {
    val stringLiteral = new StringLiteral
    stringLiteral.setValue(value)
    stringLiteral.setQuoteCharacter(quoteChar)
    stringLiteral
  }

  private def assignmentNodes(
    statementConstructor: StatementConstructor,
    indexedDeclarationTargets: IndexedDeclarationTargets,
    initializer: AstNode
  ): ArrayBuffer[AstNode] = {
    def leftLiteralWithOtherDeclarations(
      indexed: IndexedDeclarationTargets
    ): (AstNode, ArrayBuffer[AstNode]) = {
      val variableDeclarationNodes = new mutable.ArrayBuffer[AstNode]()

      def addTransitiveDeclarations(declarationsFromIndex: DeclarationTargets): Unit = {
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

      val indices = indexed.map { case (key: NumberKey, _) => tryToInt(key.value); case _ => None }
      if (indices.forall(_.isDefined)) {
        val entries =
          indices.map(_.get).zip(indexed.values).toList.sortBy(_._1)
        val arrayLiteral = new ArrayLiteral
        for ((index, declarationTargets) <- entries) {
          while (arrayLiteral.getSize < index)
            arrayLiteral.addElement(new EmptyExpression())
          if (declarationTargets.names.isEmpty) {
            val (element, otherDeclarations) = leftLiteralWithOtherDeclarations(declarationTargets.indexed)
            arrayLiteral.addElement(element)
            variableDeclarationNodes ++= otherDeclarations
          } else {
            arrayLiteral.addElement(nameNode(declarationTargets.names(0)))
            addTransitiveDeclarations(declarationTargets)
          }
        }
        (arrayLiteral, variableDeclarationNodes)
      } else {
        val objectLiteral = new ObjectLiteral
        for ((key, declarationTargets) <- indexed) {
          val declarationsFromIndex = declarationTargets
          if (declarationsFromIndex.names.isEmpty) {
            val (element, otherDeclarations) = leftLiteralWithOtherDeclarations(declarationsFromIndex.indexed)
            objectLiteral.addElement(objectPropertyNode(key.objectKeyNode, element))
            variableDeclarationNodes ++= otherDeclarations
          } else {
            objectLiteral
              .addElement(objectPropertyNode(key.objectKeyNode, nameNode(declarationsFromIndex.names(0))))
            addTransitiveDeclarations(declarationsFromIndex)
          }
        }
        (objectLiteral, variableDeclarationNodes)
      }
    }

    def nodes(indexedDeclarationTargets: IndexedDeclarationTargets, initializer: AstNode) = {
      val (target, other) = leftLiteralWithOtherDeclarations(indexedDeclarationTargets)
      other.prepend(statementConstructor(target, initializer))
      other
    }

    nodes(indexedDeclarationTargets, initializer)
  }

  sealed trait Key {
    def objectKeyNode: AstNode
  }

  case class NumberKey(value: String) extends Key {
    def objectKeyNode: AstNode = numberLiteralNode(value)
  }

  case class StringKey(value: String, quoteChar: Char) extends Key {
    override def objectKeyNode: AstNode = stringLiteralNode(value, quoteChar)
  }

  case class NameKey(name: String) extends Key {
    override def objectKeyNode: AstNode = nameNode(name)
  }

  type IndexedDeclarationTargets = mutable.HashMap[Key, DeclarationTargets]

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

  private def visitBlock(blockNode: Node): Unit = {
    val delayedDeclarations = new mutable.LinkedHashMap[Delayed, IndexedDeclarationTargets]

    def indexDeclarationTargets(indexedDeclarationTargets: IndexedDeclarationTargets, key: Key) = {
      indexedDeclarationTargets
        .getOrElseUpdate(key, DeclarationTargets(mutable.ArrayBuffer(), new IndexedDeclarationTargets))
    }

    def buildVariableDeclarations() = {
      delayedDeclarations.flatMap({ case (identifier, declarationsFromIndices) =>
        assignmentNodes(identifier.statementType, declarationsFromIndices, nameNode(identifier.initializerName))
      }).toList
    }

    def finalizeDeclarations(afterNode: Node) {
      buildVariableDeclarations().foreach(blockNode.addChildBefore(_, afterNode))
      delayedDeclarations.clear()
    }

    def visitAssignment(node: Node, statementConstructor: StatementConstructor, left: AstNode, right: AstNode) {
      left match {
        case targetName: Name =>
          def addToIndexed(indexedDeclarationTargets: IndexedDeclarationTargets, indices: List[Key]): Boolean = {
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

          def visitInitializer(indices: List[Key], initializer: AstNode): Boolean = {
            initializer match {
              case propertyGet: PropertyGet =>
                visitInitializer(NameKey(propertyGet.getProperty.getIdentifier) :: indices, propertyGet.getTarget)
              case elementGet: ElementGet =>
                visitInitializer((elementGet.getElement match {
                  case numberLiteral: NumberLiteral => NumberKey(numberLiteral.getValue)
                  case name: Name => NameKey(name.getIdentifier)
                  case stringLiteral: StringLiteral =>
                    StringKey(stringLiteral.getValue, stringLiteral.getQuoteCharacter)
                }) :: indices, elementGet.getTarget)
              case name: Name =>
                addToIndexed(
                  delayedDeclarations.getOrElseUpdate(
                    Delayed(statementConstructor, name.getIdentifier),
                    new IndexedDeclarationTargets,
                  ),
                  indices,
                )
              case _ =>
                finalizeDeclarations(node)
                val indexedDeclarationTargets = new IndexedDeclarationTargets
                if (addToIndexed(indexedDeclarationTargets, indices)) {
                  assignmentNodes(statementConstructor, indexedDeclarationTargets, initializer)
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
    delayedDeclarations.clear()
  }
}
