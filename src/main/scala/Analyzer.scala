// =====================================================================
// Ejercicios 3 y 5: Detección y conteo de entidades
// =====================================================================

/** Responsable de detectar entidades nombradas en texto libre y producir
  * estadísticas sobre ellas.
  */
import scala.util.matching.Regex
object Analyzer {

  /** Detecta las entidades del diccionario que aparecen en el texto dado.
    *
    * @param text
    *   texto a analizar (ej: título o cuerpo de un post)
    * @param dictionary
    *   lista de entidades conocidas (cargadas desde los diccionarios)
    * @return
    *   lista de entidades cuyo texto aparece en el texto analizado
    *
    * Ejercico 3:
    */
  def detectEntities(
      text: String,
      dictionary: List[NamedEntity]
  ): List[NamedEntity] = {
    val lowerText = text.toLowerCase
    dictionary
      .filter(entity => {
        val escaped = Regex.quote(entity.text.toLowerCase)
        val pattern = s"(?<![\\w])$escaped(?![\\w])"
        pattern.r.findFirstIn(lowerText).isDefined
      })
      .distinctBy(entity => (entity.text, entity.entityType))
  }

  /** Cuenta cuántas entidades de cada tipo fueron detectadas.
    *
    * @param entities
    *   lista de entidades detectadas
    * @return
    *   mapa de entityType → cantidad de apariciones
    *
    * TODO (Ejercicio 5): Implementar este método.
    *
    * Ejemplo: entities = List( Person("Alan Turing"),
    * ProgrammingLanguage("Scala"), Person("Ada Lovelace"), University("MIT") )
    * resultado = Map( "Person" -> 2, "ProgrammingLanguage" -> 1, "University"
    * -> 1 )
    */
  def countByType(entities: List[NamedEntity]): Map[String, Int] = {
    entities
      .groupBy(_.entityType)
      .map { case (tipo, lista) => (tipo, lista.length) }
      .toMap[String, Int]
  }
}
