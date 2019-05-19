import org.mozilla.javascript.Node
import org.mozilla.javascript.ast._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object DestructuringAssignmentsExtractor {

  private object IntElementGet {
    def unapply(node: Node): Option[(Node, Int)] =
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

    def finalizeDeclarations(afterNode: Node) {
      def nameNode(identifier: String) = {
        val name = new Name
        name.setIdentifier(identifier)
        name
      }

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
                  declarationsFromIndex.names(0),
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

      def arrayDeclarationNodes(declarationsFromIndices: IndexedDeclarationTargets, identifier: String) = {
        val (target, variableDeclarations) = arrayLiteralWithOtherDeclarations(declarationsFromIndices)
        variableDeclarations.prepend(variableDeclarationNode(target, nameNode(identifier)))
        variableDeclarations
      }

      for (declarationsFromIndices <- varDeclarationsFromArrayVariables) {
        arrayDeclarationNodes(declarationsFromIndices._2, declarationsFromIndices._1)
          .foreach(node.addChildBefore(_, afterNode))
      }
      varDeclarationsFromArrayVariables.clear()
    }

    node.forEach({
      case variableDeclaration: VariableDeclaration =>
        if (variableDeclaration.isVar && variableDeclaration.getVariables.size() == 1) {
          val variableInitializer = variableDeclaration.getVariables.get(0)
          variableInitializer.getTarget match {
            case targetName: Name =>
              def visitInitializer(indices: List[Int], initializer: Node): Boolean = {
                initializer match {
                  case IntElementGet(target, index) => return visitInitializer(index :: indices, target)
                  case name: Name =>
                    var indexedDeclarationTargets = varDeclarationsFromArrayVariables
                      .getOrElseUpdate(name.getIdentifier, new mutable.ArrayBuffer())
                    var declarationTargets: DeclarationTargets = null
                    for (index <- indices) {
                      declarationTargets = indexDeclarationTargets(indexedDeclarationTargets, index)
                      indexedDeclarationTargets = declarationTargets.indexed
                    }
                    if (declarationTargets != null) {
                      declarationTargets.names.append(targetName.getIdentifier)
                      return true
                    }
                  case _ =>
                }
                false
              }

              if (visitInitializer(List.empty, variableInitializer.getInitializer))
                node.removeChild(variableDeclaration)
            case _ =>
          }
        } else {
          finalizeDeclarations(variableDeclaration)
        }
      case _ =>
    })
    finalizeDeclarations(node.getLastChild)
  }
}
