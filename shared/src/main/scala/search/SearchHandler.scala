package search

import client.Client
import doc.{DocTransaction, DocUpdate}
import util.ObservableProperty
import com.softwaremill.quicklens._
import model.range.IntRange
import monix.reactive.Observable



trait SearchInterface {
  def searchState: Observable[SearchState]

}

trait StartSearchInterface {

  def startSearch(exitUiOnCommit: Boolean, direction: Int): Unit
  def repeatLastSearch(ops: Boolean): Unit
}

case class SearchState(
  searching: Option[Search],
) {
}

class SearchHandler(client: Client) extends SearchInterface with StartSearchInterface {


  private val searchState_ = ObservableProperty[SearchState](SearchState(None))

  private var lastSearch: Search = Search("")
  private var lastState: Search = lastSearch


  def searchState: ObservableProperty[SearchState] = searchState_

  def updateConstructingSearchTerm(a: String): Unit = {
    val uncommited = searchState_.get.searching.get.copy(term = a)
    lastState = uncommited
    searchState_.modify(_.copy(searching = Some(uncommited)))
  }

  def doSearch(search: Search): Unit = {
    client.state.searchInShown(search, client.enableModal).headOption.foreach {
      case SearchOccurrence(node, ocr) =>
        val nor = client.state.node(node).content match {
          case model.data.Content.Rich(content) => content.rangeAfter(ocr.start)
          case _ => IntRange(0, 0)
        }
        if (client.enableModal) {
          val mode = client.state.mode0 match {
            case model.mode.Node.Content(cn, a) =>
              a match {
                case model.mode.Content.RichVisual(a, b) =>
                  if (cn == node) {
                    model.mode.Node.Content(node, model.mode.Content.RichVisual(a, nor))
                  } else {
                    model.mode.Node.Visual(cn, node)
                  }
                case _ =>
                  model.mode.Node.Content(node, model.mode.Content.RichNormal(nor))
              }
            case model.mode.Node.Visual(fix, move) =>
              model.mode.Node.Visual(fix, node)
          }
          client.localChange(DocTransaction(mode))
        } else {
          // this will produce a range selection
          client.onMouseFocusOn(node, Some(ocr), true, false, false)
        }
    }
  }

  def repeatLastSearch(ops: Boolean): Unit = {
    if (lastSearch != null) {
      doSearch(if (ops) lastSearch.reverse else lastSearch)
    }
  }

  def commitSearching(force: Boolean = false): Unit = {
    if (lastState.term.nonEmpty) {
      lastSearch = lastState
      doSearch(lastSearch)
      // go to first highlighted mark
      if (exitOnCommit || force) {
        searchState_.update(SearchState(None))
      }
    }
  }

  private var exitOnCommit = false

  def cancelSearching(): Unit = {
    searchState_.update(SearchState(None))
  }

  def startSearch(exitUiOnCommit: Boolean, direction: Int): Unit = {
    exitOnCommit = exitUiOnCommit
    lastState = lastState.copy(direction = direction)
    // when start search, we don't actually show the old term in view
    searchState_.update(SearchState(Some(lastState.copy(term = ""))))
  }
}
