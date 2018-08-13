package web.view

import client.Client
import _root_.doc.DocTransaction
import model.cursor
import model.cursor.Node
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import web.view.content.ContentView

class QuickSearchDialog(val client: Client,
  override val layer: OverlayLayer,
  override protected val covering: () => HTMLElement
) extends FilteringView[model.cursor.Node] with  CoveringOverlay  {


  override protected val search = input(
    width := "100%",
    `class` := "ct-input ct-input-large"
  ).render

  override protected val list = div(
    overflowY := "scroll",
    overflowX := "hidden",
    flex := "1 1 auto",
    width := "100%",
    color := "#cccccc",
    `class` := "ct-scroll"
  ).render

  dom = div(
    `class` := "ct-card",
    display := "flex",
    flexDirection := "column",
    div(width := "100%", padding := "10px", search),
    list
  ).render

  override def data(term: String): Seq[model.cursor.Node] = {
    val tt = term.split("\\s").filter(_.length > 2)
    if (tt.isEmpty) {
      Seq.empty
    } else {
      client.state.quickSearch(tt)
    }
  }


  override def destroyItem(a: HTMLElement): Unit = {
    View.fromDom[ContentView.General](a).destroy()
  }

  override def renderItem(t: model.cursor.Node, index: Int): HTMLElement = {
    ContentView.create(client.state.node(t).content).dom
  }

  override def onSelected(t: model.cursor.Node): Unit = {
    val n = client.state.node(t)
    client.localChange(DocTransaction(Seq.empty,
      Some(model.mode.Node(t, n.content.defaultNormalMode())),
      zoomAfter = Some(t)))
  }
}
