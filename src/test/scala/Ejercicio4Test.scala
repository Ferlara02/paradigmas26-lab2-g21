import Dictionary.loadAll
class Ejercicio4_formatNERResult extends munit.FunSuite {
  test("formatNERResult works correctly with standard dictionaries") {

    val text = "Scala 3 released at EPFL by Martin Odersky"
    val dictionary = loadAll()
    val entities = Analyzer.detectEntities(text, dictionary)

    val expected = "Post: \"Scala 3 released at EPFL by Martin Odersky\"\n" +
      "Entidades detectadas:\n" +
      "  [ProgrammingLanguage] Scala\n" +
      "  [University] EPFL\n" +
      "  [Person] Martin Odersky\n"

    val obtained = Formatters.formatNERResult(text, entities)
    assertEquals(obtained, expected)
  }

  test("formatNERResult works correctly with empty dictionaries") {
    val text = "Scala 3 released at EPFL by Martin Odersky"
    val dictionary = Nil
    val entities = Analyzer.detectEntities(text, dictionary)

    val expected = "Post: \"Scala 3 released at EPFL by Martin Odersky\"\n" +
      "  (sin entidades detectadas)\n"

    val obtained = Formatters.formatNERResult(text, entities)
    assertEquals(obtained, expected)
  }

  test("formatNERResult works correctly with an empty post title") {
    val text = ""
    val dictionary = loadAll()
    val entities = Analyzer.detectEntities(text, dictionary)

    val expected = "Post: \"\"\n" +
      "  (sin entidades detectadas)\n"

    val obtained = Formatters.formatNERResult(text, entities)
    assertEquals(obtained, expected)
  }

  test("formatNERResult works correctly with an empty post title") {
    val text = ""
    val dictionary = loadAll()
    val entities = Analyzer.detectEntities(text, dictionary)

    val expected = "Post: \"\"\n" +
      "  (sin entidades detectadas)\n"

    val obtained = Formatters.formatNERResult(text, entities)
    assertEquals(obtained, expected)
  }
}

class Ejercicio4_formatNERResult2 extends munit.FunSuite {}
