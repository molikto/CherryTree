package web.ui

import command.Key._
import scalatags.JsDom.all._
import search.{Search, SearchHandler}
import web.view._
import web.ui._
import org.scalajs.dom._

class SearchBar(
  searchHandler: SearchHandler, doc: () => View, paddingBottom: Int) extends View {


  private val search = input(
    `class` := "ct-input",
  ).render

  dom = div(
    `class` := "ct-card",
    div(width := "100%", padding := "6px", search),
    minWidth := "250px",
    position := "absolute",
    left := "24px",
    bottom := (paddingBottom - 2) + "px"
  ).render

  var dismissed = false


  event(search, "input", (e: Event) => {
    searchHandler.updateConstructingSearchTerm(search.textContent)
  })

  event(search, "keydown", (k: KeyboardEvent) => {
    KeyMap.get(k.key) match {
      case Some(Enter) =>
        searchHandler.commitConstructing()
        k.preventDefault()
      case Some(Escape) =>
        searchHandler.cancelConstructing()
        k.preventDefault()
      case _ =>
    }
  })

  override def focus(): Unit = {
    search.focus()
  }

  def show(): Unit = {
    if (dismissed) {
      dismissed = false
      dom.style.display = "block"
    }
  }

  def dismiss(): Unit = {
    if (!dismissed) {
      dismissed = true
      dom.style.display = "none"
    }
  }

  observe(searchHandler.searchState.doOnNext(state => {
    // we ignore all other updates here because they will be all from us
    if (state.uiShown && dismissed) {
      show()
      focus()
      search.select()
    } else if (!state.uiShown && !dismissed) {
      dismiss()
      doc().focus()
    }
  }))
}
