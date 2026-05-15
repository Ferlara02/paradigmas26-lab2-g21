// =====================================================================
// Ejercicios 4 y 5: Formateo de resultados
// =====================================================================

/** Responsable de convertir los resultados del análisis a texto para mostrar.
  */
object Formatters {

  /** Formatea el análisis NER de un post individual.
    *
    * @param postTitle
    *   título del post analizado
    * @param entities
    *   entidades detectadas en ese post
    * @return
    *   bloque de texto con el título y las entidades encontradas
    *
    * TODO (Ejercicio 4): Implementar este método.
    *
    * Usar el método describe de cada entidad para generar la salida. No es
    * necesario hacer match sobre el tipo concreto de cada entidad: describe ya
    * funciona correctamente para cualquier subtipo (polimorfismo).
    *
    * Ejemplo de salida esperada:
    *
    * Post: "Scala 3 released at EPFL by Martin Odersky" Entidades detectadas:
    * [ProgrammingLanguage] Scala [University] EPFL [Person] Martin Odersky
    *
    * Si no se detectaron entidades, mostrar un mensaje indicándolo.
    */
  def formatNERResult(
      postTitle: String,
      entities: List[NamedEntity]
  ): String = {
    val sorted = entities.sortWith((a, b) => // orden por aparición en postTitle
      postTitle.indexOf(a.text) < postTitle.indexOf(b.text)
    )
    val formattedEntities = sorted.map(e => "  " + e.describe).mkString("\n")
    val ansv1 = s"Post: \"$postTitle\"\n" +
      s"Entidades detectadas:\n" +
      s"$formattedEntities" + "\n"

    val ansv2 = s"Post: \"$postTitle\"\n" + s"  (sin entidades detectadas)\n"

    sorted match {
      case Nil => ansv2
      case _   => ansv1
    }
  }

  def formatOrganization(counts: Map[String, Int]): String = {

    // Filtro los contadores que quiero
    val filtered = counts.filter { case (entityType, _) =>
      entityType == "Organization" || entityType == "University"
    }

    val orgCount = counts.getOrElse("Organization", 0)
    val univCount = counts.getOrElse("University", 0)
    // Verifico si hay casos de no universidades
    val directa = orgCount - univCount

    // Verifico si existe caso de University
    val existeUniv = filtered.contains("University")

    if (existeUniv) {
      val directaStr =
        if (directa > 0) s"(Organization directa): $directa" else ""
      s"Organization: $orgCount\n" +
        s"  University: $univCount\n" +
        s"  $directaStr"
    } else {
      s"Organization: $orgCount"
    }
  }

  def formatTechnology(counts: Map[String, Int]): String = {
    val filtered = counts.filter { case (entityType, _) =>
      entityType == "Technology" || entityType == "ProgrammingLanguage"
    }

    // Verifico si existe caso de ProgrammingLanguage
    val existePL = filtered.contains("ProgrammingLanguage")

    val ans: String = existePL match {
      case true =>
        s"Technology: ${filtered.getOrElse("Technology", 0)} \n" +
          s"  ProgrammingLanguage: ${filtered.getOrElse("ProgrammingLanguage", 0)}\n"
      case false => s"Technology: ${filtered.get("Technology")}\n"
    }
    ans
  }

  /** Formatea un resumen de estadísticas de entidades por tipo.
    *
    * @param counts
    *   mapa de entityType → cantidad
    * @return
    *   texto con las estadísticas ordenadas por cantidad (de mayor a menor)
    *
    * TODO (Ejercicio 5): Implementar este método.
    *
    * Ejemplo de salida esperada:
    *
    * ===Estadísticas de entidades===
    * Person: 5 ProgrammingLanguage: 3 Organization: 2 University: 2
    */
  def formatEntityStats(counts: Map[String, Int]): String = {

    // Valores que se deben mergear
    val merges = List(
      ("Organization", "University"),
      ("Technology", "ProgrammingLanguage")
    )

    // Suma valores, si no existian instancias de padre crea index key/value total del hijo
    val newCounts = merges.foldLeft(counts) { case (acc, (target, source)) =>
      if (!acc.contains(target) && acc.getOrElse(source, 0) > 0)
        acc + (target -> acc(source))
      else
        acc
    }

    // Me quedo solo con hijos de EntityName, los paso a lista y los ordeno
    val filteredNewCounts = newCounts
      .filter { case (k, v) => k != "University" && k != "ProgrammingLanguage" }
      .toList
      .sortBy { case (entityType, count) => (-count, entityType) }

    val formattedEntities = filteredNewCounts
      .map {
        case ("Organization", _) => formatOrganization(newCounts)
        case ("Technology", _)   => formatTechnology(newCounts)
        case (entityType, count) => s"$entityType: $count"
      }
      .mkString("\n")

    formattedEntities match {
      case "" => s"=== Estadísticas de entidades ===\n"
      case _ =>
        s"=== Estadísticas de entidades ===\n" + formattedEntities + "\n"
    }
  }
}
