import Formatters.formatEntityStats

class Ejercicio5_countByType extends munit.FunSuite {

  test("countByType with empty list returns empty map") {
    assertEquals(Analyzer.countByType(List.empty), Map.empty[String, Int])
  }

  test("countByType returns correct count per entity type") {
    val entities = List(
      new Person("Alan Turing"),
      new ProgrammingLanguage("Scala"),
      new Person("Ada Lovelace"),
      new University("MIT")
    )

    val resultado = Analyzer.countByType(entities)

    assertEquals(resultado("Person"), 2)
    assertEquals(resultado("ProgrammingLanguage"), 1)
    assertEquals(resultado("University"), 1)
    assert(!resultado.contains("Organization"))
  }
}

class Ejercicio5_formatEntityStats extends munit.FunSuite {
  test("formatEntityStats works correctly") {
    val expected = "=== Estadísticas de entidades ===\n" +
      "Person: 5\n" +
      "ProgrammingLanguage: 3\n" +
      "Organization: 2\n" +
      "University: 2\n"

    val entities = List(
      new Person("Ada Lovelace"),
      new Person("Charles Babbage"),
      new Person("Alan Turing"),
      new Person("Grace Hopper"),
      new Person("John von Neumann"),
      new ProgrammingLanguage("Scala"),
      new ProgrammingLanguage("C"),
      new ProgrammingLanguage("Python"),
      new University("UNC"),
      new University("MIT"),
      new Organization("IBM"),
      new Organization("Alphabet")
    )
    val counts = Analyzer.countByType(entities)

    val obtained = formatEntityStats(counts)
    assertEquals(obtained, expected)

  }

  test("formatEntityStats with empty map returns only header") {
    val expected = "=== Estadísticas de entidades ===\n"
    val obtained = formatEntityStats(Map.empty[String, Int])
    assertEquals(obtained, expected)
  }
}
