package search


sealed trait SearchBehaviour {

}
object SearchBehaviour {
  case object GraphemeSegments extends SearchBehaviour
}

case class Search(
  term: String,
  regex: Boolean,
  ignoreCase: Boolean,
  behaviour: SearchBehaviour = SearchBehaviour.GraphemeSegments
)
