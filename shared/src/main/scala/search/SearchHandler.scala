package search

import client.Client
import doc.DocTransaction
import util.ObservableProperty
import com.softwaremill.quicklens._
import monix.reactive.Observable



trait SearchInterface {
  def searchState: Observable[SearchState]

}

trait StartSearchInterface {

  def startSearch(exitUiOnCommit: Boolean, direction: Int): Unit
}

case class SearchState(
  searching: Option[Search],
) {
}

trait SearchHandler extends SearchInterface with StartSearchInterface { self: Client =>


  private val searchState_ = ObservableProperty[SearchState](SearchState(None))

  private var lastSearch: Search = Search("")
  private var lastState: Search = lastSearch


  def searchState: ObservableProperty[SearchState] = searchState_

  def updateConstructingSearchTerm(a: String): Unit = {
    val uncommited = searchState_.get.searching.get.copy(term = a)
    lastState = uncommited
    searchState_.modify(_.copy(searching = Some(uncommited)))
  }

  def commitSearching(): Unit = {
    lastSearch = lastState
    state.searchInShown(lastState).headOption.foreach {
      case SearchOccurrence(node, ocr) =>
        if (enableModal) {
        } else {
          onMouseFocusOn(node, ocr, true, false, false)
        }
    }
    // go to first highlighted mark
    if (exitOnCommit) {
      searchState_.update(SearchState(None))
    }
  }

  private var exitOnCommit = false

  def cancelSearching(): Unit = {
    searchState_.update(SearchState(None))
  }

  def startSearch(exitUiOnCommit: Boolean, direction: Int): Unit = {
    exitOnCommit = exitUiOnCommit
    searchState_.update(SearchState(Some(lastState.copy(direction = direction, term = ""))))
  }
}
