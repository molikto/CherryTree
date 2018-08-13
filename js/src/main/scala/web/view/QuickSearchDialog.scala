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
) extends FilteringView[(model.cursor.Node, String)] with  CoveringOverlay with UnselectableView {


  override protected val search = input(
    width := "100%",
    `class` := "ct-input ct-input-large"
  ).render

  override protected val list = div(
    flex := "1 1 auto",
    color := "#cccccc",
    width := "100%",
    height := "100%",
    `class` := "ct-scroll ct-document-style"
  ).render


  dom = div(
    `class` := "ct-card",
    display := "flex",
    flexDirection := "column",
    div(width := "100%", padding := "10px", search),
    list
  ).render

  override def data(term: String): Seq[(model.cursor.Node, String)] = {
    val tt = term.split("\\s").filter(a => !util.isAscii(a) || a.length > 2)
    if (tt.isEmpty) {
      Seq.empty
    } else {
      client.state.quickSearch(tt).map(a => (a, client.state.node(a).uuid))
    }
  }


  override def destroyItem(a: HTMLElement): Unit = {
    View.fromDom[ContentView.General](a.childNodes(0)).destroy()
    super.destroyItem(a)
  }

  override def renderItem(t: (model.cursor.Node, String), index: Int): HTMLElement = {
    div(
      `class` := "ct-menu-item ",
      ContentView.create(client.state.node(t._1).content).dom
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
      zoomAfter = Some(n)))
  }
}
