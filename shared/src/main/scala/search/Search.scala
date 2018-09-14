package search

import model.data.Unicode
import model.range.IntRange


sealed trait SearchBehaviour {

}
object SearchBehaviour {
  case object UnicodeSegments extends SearchBehaviour
}

case class SearchOccurrence(
  node: model.cursor.Node,
  range: IntRange

) {

  def +(a: Int): SearchOccurrence = copy(range = range.moveBy(a))
}

case class Search(
  term: String,
  regex: Boolean = false,
  ignoreCase: Boolean = true,
  direction: Int = 0,
  behaviour: SearchBehaviour = SearchBehaviour.UnicodeSegments
) {
  def reverse: Search = if (direction >= 0) copy(direction = -1) else copy(direction = 1)

  val lowerTerm: Unicode = Unicode(term).lower
}
