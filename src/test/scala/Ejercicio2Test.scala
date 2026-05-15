import Dictionary.loadFromFile
object Check { // ad hoc data

  val persons = loadFromFile("data/places.txt", "Person")
  val universities = loadFromFile("data/places.txt", "University")
  val pl = loadFromFile("data/places.txt", "ProgrammingLanguage")
  val organizations = loadFromFile("data/places.txt", "Organization")
  val places = loadFromFile("data/places.txt", "Place")

  val totalIntance = 65
  val fileNumber = 5
  val fEnt = "fakeEntity"

}

class Ejercicio2_loadFromFile extends munit.FunSuite {

  // TODO: verificar que se cierre todo

  test("loadFromFile with unknown entityType returns empty list") {
    val t = loadFromFile("data/places.txt", Check.fEnt)
    assertEquals(t, List.empty)
  }

  test("loadFromFile creates correct entity type") {
    assert(Check.persons.forall(e => e.describe.contains("Person")))
    assert(Check.universities.forall(e => e.describe.contains("University")))
    assert(Check.pl.forall(e => e.describe.contains("ProgrammingLanguage")))
    assert(Check.organizations.forall(e => e.describe.contains("Organization")))
    assert(Check.places.forall(e => e.describe.contains("Place")))
  }

}

class Ejercicio2_loadAll extends munit.FunSuite {

  val allInstances = Dictionary.loadAll()

  test("loadAll returns non empty list") {
    assertNotEquals(allInstances.length, 0)
  }

  test("loadAll returns at least totalIntance entities") {
    assertEquals(
      (allInstances.length > 0 && allInstances.length < Check.totalIntance),
      false
    )
  }

  test("no entity has empty text") {
    assert(allInstances.forall(e => e.text.nonEmpty))
  }

  /* La implementacion de entityType permite dos objetos identicos pero diferentes
    test("no duplicate entities in loadAll") {
        assertEquals(allInstances.length, allInstances.distinct.length)
    }
   */

  test("loadAll excludes file headers and returns exact entity count") {
    assertNotEquals(allInstances.length, Check.totalIntance + Check.fileNumber)
    assert(!allInstances.exists(e => e.describe.contains("#")))
    assertEquals(allInstances.length, Check.totalIntance)
  }

  test("loadAll contains all entity types") {
    val types = allInstances.map(_.entityType).toSet
    assert(types.contains("Person"))
    assert(types.contains("University"))
    assert(types.contains("Place"))
    assert(types.contains("ProgrammingLanguage"))
    assert(types.contains("Organization"))
  }
}
