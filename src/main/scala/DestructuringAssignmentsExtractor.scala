import org.mozilla.javascript.Node
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

  private def arrayDeclarationNodes(declarationsFromIndices: IndexedDeclarationTargets, initializer: AstNode): ArrayBuffer[VariableDeclaration] = {
    def arrayLiteralWithOtherDeclarations(
      declarationsFromIndices: IndexedDeclarationTargets
    ): (ArrayLiteral, ArrayBuffer[VariableDeclaration]) = {
      val arrayLiteral = new ArrayLiteral
      val variableDeclarationNodes = new mutable.ArrayBuffer[VariableDeclaration]()
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
                .append(variableDeclarationNode(nameNode(alias), nameNode(declarationsFromIndex.names(0))))
            }
            if (declarationsFromIndex.indexed.nonEmpty)
              variableDeclarationNodes.appendAll(arrayDeclarationNodes(
                declarationsFromIndex.indexed,
                nameNode(declarationsFromIndex.names(0)),
              ))
          }
        }
      }
      (arrayLiteral, variableDeclarationNodes)
    }

    def variableDeclarationNode(target: AstNode, initializer: AstNode) = {
      val variableDeclaration = new VariableDeclaration()
      val variableInitializer = new VariableInitializer()
      variableInitializer.setInitializer(initializer)
      variableInitializer.setTarget(target)
      variableDeclaration.addVariable(variableInitializer)
      variableDeclaration.setIsStatement(true)
      variableDeclaration
    }

    val (target, variableDeclarations) = arrayLiteralWithOtherDeclarations(declarationsFromIndices)
    variableDeclarations.prepend(variableDeclarationNode(target, initializer))
    variableDeclarations
  }

  type IndexedDeclarationTargets = mutable.ArrayBuffer[DeclarationTargets]

  case class DeclarationTargets(names: mutable.ArrayBuffer[String], indexed: IndexedDeclarationTargets)

  private def visitExtractable(node: Node): Unit = {
    val varDeclarationsFromArrayVariables = new mutable.LinkedHashMap[String, IndexedDeclarationTargets]

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
        arrayDeclarationNodes(declarationsFromIndices, nameNode(identifier))
      }).toList
    }

    def finalizeDeclarations(afterNode: Node) {
      buildVariableDeclarations().foreach(node.addChildBefore(_, afterNode))
      varDeclarationsFromArrayVariables.clear()
    }

    node.forEach({
      case variableDeclaration: VariableDeclaration =>
        if (variableDeclaration.isVar && variableDeclaration.getVariables.size() == 1) {
          val variableInitializer = variableDeclaration.getVariables.get(0)
          variableInitializer.getTarget match {
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
                      varDeclarationsFromArrayVariables
                        .getOrElseUpdate(name.getIdentifier, new mutable.ArrayBuffer()),
                      indices,
                    )
                  case _ =>
                    finalizeDeclarations(variableDeclaration)
                    val indexedDeclarationTargets = new IndexedDeclarationTargets
                    if (addToIndexed(indexedDeclarationTargets, indices)) {
                      arrayDeclarationNodes(indexedDeclarationTargets, initializer)
                        .foreach(node.addChildBefore(_, variableDeclaration))
                      true
                    } else
                      false
                }
              }

              if (visitInitializer(List.empty, variableInitializer.getInitializer))
                node.removeChild(variableDeclaration)
            case _ => finalizeDeclarations(variableDeclaration)
          }
        } else {
          finalizeDeclarations(variableDeclaration)
        }
      case child => finalizeDeclarations(child)
    })
    buildVariableDeclarations().foreach(node.addChildToBack)
    varDeclarationsFromArrayVariables.clear()
  }
}
