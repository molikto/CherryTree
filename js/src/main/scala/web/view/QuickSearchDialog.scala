package web.view

import client.Client
import _root_.doc.DocTransaction
import model.cursor
import model.cursor.Node
import org.scalajs.dom.raw._
import scalatags.JsDom.all.{tag, _}
import settings.Settings
import web.view.content.ContentView

class QuickSearchDialog(val client: Client,
  override val layer: OverlayLayer,
  override protected val covering: () => HTMLElement
) extends FilteringView[(model.cursor.Node, String)]
  with  CoveringOverlay
  with UnselectableView with Settings {


  var currentViewport = false
  def show(viewport: Boolean): Unit = {
    currentViewport = viewport
    super.show()
  }


  override protected val search = input(
    width := "100%",
    `class` := "ct-input ct-input-large"
  ).render

  override protected val list = div(
    flex := "1 1 auto",
    color := "#cccccc",
    width := "100%",
    height := "calc(100% - 57px)",
    `class` := "ct-scroll ct-document-style"
  ).render


  dom = div(
    `class` := "ct-card",
    display := "flex",
    flexDirection := "column",
    div(width := "100%", padding := "10px", search),
    list
  ).render


  def complexTerm(str: String): Boolean = {
    if (str.isEmpty) {
      false
    } else if (str.length <= 1) {
      !util.isEnglishLetter(str.codePointAt(0))
    } else {
      true
    }
  }

  override def data(term: String): Seq[(model.cursor.Node, String)] = {
    val tt = term.split("\\s").toSeq.filter(complexTerm).map(a => model.data.Unicode(a.toLowerCase()))
    if (tt.isEmpty) {
      Seq.empty
    } else {
      client.state.quickSearch(tt, delimitationGraphemes, currentViewport).map(a => (a, client.state.node(a).uuid))
    }
  }


  override def destroyItem(a: HTMLElement): Unit = {
    View.fromDom[ContentView.General](a.childNodes(0).childNodes(0)).destroy()
    super.destroyItem(a)
  }

  override def renderItem(t: (model.cursor.Node, String), index: Int): HTMLElement = {
    val node = client.state.node(t._1)
    div(
      `class` := "ct-menu-item ",
      display := "flex",
      flexDirection := "row",
      div(
        `class` := doc.classesFromNodeAttribute(node),
        ContentView.create(node.content).dom
      ),
      tag("i")(`class` := "ct-d-hold")
    ).render
  }

  override def onSelected(t:  (model.cursor.Node, String)): Unit = {
    var n = t._1
    if (client.state.node(n).uuid != t._2) {
      val find = client.state.node.lookup(t._2)
      if (find.isDefined) {
        n = find.get
      } else {
        return
      }
    }
    client.localChange(DocTransaction(Seq.empty,
      Some(model.mode.Node.Content(n, client.state.node(n).content.defaultNormalMode())),
      zoomAfter = if (model.cursor.Node.contains(client.state.zoom, n) && !client.state.hidden(n)) None else Some(n)))
  }
}
