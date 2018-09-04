package search

import doc.DocTransaction
import util.ObservableProperty
import com.softwaremill.quicklens._



case class SearchState(
  lastCommitted: Option[Search],
  uiShown: Boolean,
  uncommited: Search, // only used by UI
) {
}
trait SearchHandler {

  private val searchState_ = ObservableProperty[SearchState](SearchState(None, false, Search("")))

  def searchState: ObservableProperty[SearchState] = searchState_


  def updateConstructingSearchTerm(a: String): Unit = {
    searchState_.modify(_.modify(_.uncommited).using(_.copy(term = a)))
  }

  def commitConstructing(): Unit = {
    searchState_.modify(a => SearchState(Some(a.uncommited), exitOnCommit, a.uncommited))
  }

  private var exitOnCommit = false

  def cancelConstructing(): Unit = {
    searchState_.modify(_.copy(uiShown = false))
  }

  def startSearch(exitUiOnCommit: Boolean, direction: Int): Unit = {
    exitOnCommit = exitUiOnCommit
    searchState_.modify(a => a.copy(uiShown = true, uncommited = a.uncommited.copy(direction = direction)))
  }
}
