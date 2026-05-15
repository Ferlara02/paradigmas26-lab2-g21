import scala.annotation.strictfp
class Ejercicio3_detectEntities extends munit.FunSuite {

  test("detectEntities with empty dictionary returns empty list") {
    assertEquals(
      Analyzer.detectEntities("I love Scala", List.empty),
      List.empty
    )
  }

  test("detectEntities with empty text returns empty list") {
    val dictionary = List(new Person("Alan Turing"))
    assertEquals(Analyzer.detectEntities("", dictionary), List.empty)
  }

  test(
    "detectEntities detects a name multiple times but only instantiates it once"
  ) {
    val text =
      "Pepe Pecas pica papas con un pico. Con un pico pica papas Pepe Pecas"
    val dictionary = List(
      new Person("Pepe Pecas")
    )
    val resultado = Analyzer.detectEntities(text, dictionary)
    assertNotEquals(resultado.length, 2)
    assertEquals(resultado.length, 1)
  }

  test(
    "detectEntities has a dictionary with repeated entries but only instantiates it once"
  ) {
    val text =
      "Donald Knuth is a Person who deserves more recognition at my University, alongside other remarkable academics."
    val dictionary = List(
      new Person("Donald Knuth"),
      new Person("Donald Knuth"),
      new Person("Donald Knuth")
    )
    val resultado = Analyzer.detectEntities(text, dictionary)
    assertNotEquals(resultado.length, 3)
    assertEquals(resultado.length, 1)
  }

  test(
    "detectEntities doesn't produce false positives from entityType keywords"
  ) {
    val text =
      "Donald Knuth is a Person who deserves more recognition at my University, alongside other remarkable academics."
    val dictionary = List(
      new ProgrammingLanguage("Scala"),
      new University("UNC"),
      new Person("Martin Odersky"),
      new Person("Ada Lovelace"),
      new Person("Donald Knuth")
    )
    val resultado = Analyzer.detectEntities(text, dictionary)
    assert(
      !resultado.exists(e =>
        e.text == "Martin Odersky" && e.entityType == "Person"
      )
    )
    assert(
      !resultado.exists(e =>
        e.text == "Ada Lovelace" && e.entityType == "Person"
      )
    )
  }

  test("detectEntities return correct entity detection") {
    val text = "Scala fue creado en EPFL por Martin Odersky"
    val dictionary = List(
      new ProgrammingLanguage("Scala"),
      new University("EPFL"),
      new Person("Martin Odersky"),
      new Person("Ada Lovelace")
    )
    val resultado = Analyzer.detectEntities(text, dictionary)
    resultado.map(e => println(e.describe + "\n"))
    assertEquals(resultado.length, 3)
    assert(
      resultado.exists(e =>
        e.text == "Scala" && e.entityType == "ProgrammingLanguage"
      )
    )
    assert(
      resultado.exists(e => e.text == "EPFL" && e.entityType == "University")
    )
    assert(
      resultado.exists(e =>
        e.text == "Martin Odersky" && e.entityType == "Person"
      )
    )
    assert(!resultado.exists(e => e.text == "Ada Lovelace"))
  }

  // REVISAR
  test("detectEntities matches substring") {
    val dictionary = List(new Person("Turing"), new Person("Alan Turing"))
    val resultado = Analyzer.detectEntities("Alan Turing", dictionary)
    // ambas matchean porque "Turing" está contenido en "Alan Turing"
    assertEquals(resultado.length, 2)
  }

  test("detectEntities matches whole words only") {

    val text = "C# and C++ are popular, but C is older"
    val dictionary = List(
      new ProgrammingLanguage("C#"),
      new ProgrammingLanguage("C++"),
      new ProgrammingLanguage("C")
    )
    val resultado = Analyzer.detectEntities(text, dictionary)
    assert(resultado.exists(e => e.text == "C"))
    assert(resultado.exists(e => e.text == "C#"))
    assert(resultado.exists(e => e.text == "C++"))
    assertEquals(resultado.length, 3)

    val text2 = "C# and C++ are popular, but C is older"
    val dictionary2 = List(
      new ProgrammingLanguage("C#"),
      new ProgrammingLanguage("C++")
    )
    val resultado2 = Analyzer.detectEntities(text2, dictionary2)
    assert(!resultado2.exists(e => e.text == "C"))
  }

  test(
    "detectEntities doesn't includes word segments found in the dictionary"
  ) {

    val text1 = "Java and JavaScript are popular are not the same language"
    val dictionary1 = List(
      new ProgrammingLanguage("Java")
    )
    val resultado1 = Analyzer.detectEntities(text1, dictionary1)
    assertNotEquals(resultado1.length, 2)

    val text2 = "Is JavaScript a popular ProgrammingLanguage?"
    val dictionary2 = List(
      new ProgrammingLanguage("Java")
    )
    val resultado2 = Analyzer.detectEntities(text2, dictionary2)
    assertEquals(resultado2.length, 0)

    val text3 = "Java is a popular ProgrammingLanguage"
    val dictionary3 = List(
      new ProgrammingLanguage("JavaScript")
    )
    val resultado3 = Analyzer.detectEntities(text3, dictionary3)
    assertEquals(resultado3.length, 0)
  }
}
