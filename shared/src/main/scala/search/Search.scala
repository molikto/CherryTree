package search

import model.range.IntRange


sealed trait SearchBehaviour {

}
object SearchBehaviour {
  case object GraphemeSegments extends SearchBehaviour
}

case class SearchOccurrence(
  node: model.cursor.Node,
  range: Option[IntRange]
)

case class Search(
  term: String,
  regex: Boolean = false,
  ignoreCase: Boolean = true,
  direction: Int = 0,
  behaviour: SearchBehaviour = SearchBehaviour.GraphemeSegments
)
