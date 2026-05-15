class Ejercicio1 extends munit.FunSuite {

  test("returns correct format for each entity type") {
    val entities: List[NamedEntity] = List(
      new Person("Alan Turing"),
      new University("MIT"),
      new ProgrammingLanguage("Scala"),
      new Place("San Francisco")
    )

    assertEquals(
      entities(0).describe,
      "[Person] Alan Turing",
      "Wrong implementation for class \"Person\""
    )
    assertEquals(
      entities(1).describe,
      "[University] MIT",
      "Wrong implementation for class \"University\""
    )
    assertEquals(
      entities(2).describe,
      "[ProgrammingLanguage] Scala",
      "Wrong implementation for class \"ProgrammingLanguage\""
    )
    assertEquals(
      entities(3).describe,
      "[Place] San Francisco",
      "Wrong implementation for class \"Place\""
    )
  }

  test("University is a subclass of Organization") {
    val mit = new University("MIT")
    assertEquals(mit.entityType, "University")
    assertEquals(
      mit.isInstanceOf[Organization],
      true,
      "\"University\" should be a subclass of \"Organization\""
    )
  }

  test("ProgrammingLanguage is a subclass of Technology") {
    val scl = new ProgrammingLanguage("Scala")
    assertEquals(scl.entityType, "ProgrammingLanguage")
    assertEquals(
      scl.isInstanceOf[Technology],
      true,
      "\"ProgrammingLanguage\" should be a subclass of \"Technology\""
    )
  }

  test("Liskov substitution principle") {
    def printDescription(e: NamedEntity): String = e.describe

    assertEquals(printDescription(new Person("Juan")), "[Person] Juan")
    assertEquals(printDescription(new University("UNC")), "[University] UNC")
  }
}
