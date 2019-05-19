import java.io.File
import java.nio.file.{Files, Paths}

import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("testMain") {
    val path = File.createTempFile("ping", ".js").getPath
    reflect.io.File(path).writeAll("function ping(arr) {\n  var a = arr[0];\n  var b = arr[1];\n}\n")
    Main.main(Array(path))
    assertResult(
      "function ping(arr) {\n  var [a, b] = arr;\n}\n"
    )(new String(Files.readAllBytes(Paths.get(path.replaceAll("\\.js$", "") + ".out.js"))))
  }
}
