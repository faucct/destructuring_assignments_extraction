import java.io.{File, FileReader, FileWriter}

import org.mozilla.javascript.{CompilerEnvirons, Parser}

import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]): Unit = {
    for (arg <- args) {
      val source = new File(arg)
      val target = new File(arg.replaceAll("\\.js$", "") + ".out.js")
      val environment = new CompilerEnvirons
      environment.setRecordingComments(true)
      withResources(new FileReader(source)) { reader =>
        withResources(new FileWriter(target)) { writer =>
          val tree = new Parser(environment).parse(reader, null, 0)
          DestructuringAssignmentsExtractor(tree)
          writer.write(tree.toSource())
        }
      }
    }
  }

  private def withResources[T <: AutoCloseable, V](r: => T)(f: T => V): V = {
    val resource: T = r
    require(resource != null, "resource is null")
    var exception: Throwable = null
    try {
      f(resource)
    } catch {
      case NonFatal(e) =>
        exception = e
        throw e
    } finally {
      closeAndAddSuppressed(exception, resource)
    }
  }

  private def closeAndAddSuppressed(e: Throwable,
    resource: AutoCloseable): Unit = {
    if (e != null) {
      try {
        resource.close()
      } catch {
        case NonFatal(suppressed) =>
          e.addSuppressed(suppressed)
      }
    } else {
      resource.close()
    }
  }
}
