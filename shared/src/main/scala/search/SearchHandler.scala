package search

import doc.DocTransaction
import util.ObservableProperty
import com.softwaremill.quicklens._



case class SearchState(
  lastCommitted: Option[Search],
  uiShown: Boolean,
  fromUser: Boolean,
  uncommited: Search,
) {
}
trait SearchHandler {

  private val searchState_ = ObservableProperty[SearchState](SearchState(None, false, false, Search("", false, true)))

  def searchState: ObservableProperty[SearchState] = searchState_


  def updateConstructingSearchTerm(a: String): Unit = {
    searchState_.modify(_.modify(_.fromUser).using(_ => true).modify(_.uncommited).using(_.copy(term = a)))
  }

  def commitConstructing(): Unit = {
    searchState_.modify(a => SearchState(Some(a.uncommited), false, exitOnCommit, a.uncommited))
  }

  private var exitOnCommit = false

  def cancelConstructing(): Unit = {
    searchState_.modify(_.copy(uiShown = false, fromEdit = false))
  }

  def startSearch(exitUiOnCommit: Boolean): Unit = {
    exitOnCommit = exitUiOnCommit
    searchState_.modify(_.copy(uiShown = true, fromEdit = false))
  }
}
