import org.mozilla.javascript.Parser

class DestructuringAssignmentsExtractorTest extends org.scalatest.FunSuite {
  test("variable declarations") {
    val script = parser().parse(
      "function ping(arr) {\n  var a = arr[0];\n  var b = arr[1];\n}\n",
      null,
      0
    )
    DestructuringAssignmentsExtractor(script)
    assertResult("function ping(arr) {\n  var [a, b] = arr;\n}\n")(script.toSource(0))
  }

  private def parser(): Parser = {
    import org.mozilla.javascript.CompilerEnvirons

    val environment = new CompilerEnvirons
    environment.setRecordingComments(true)
    new Parser(environment)
  }
}
