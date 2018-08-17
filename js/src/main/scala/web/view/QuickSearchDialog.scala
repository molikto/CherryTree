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
  val coveringElement: HTMLElement
) extends FilteringView[Boolean, (model.cursor.Node, String)]
  with UnselectableView with Settings {


  override def show(t: Boolean): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    super.show(t)
  }

  override protected val search = input(
    width := "100%",
    `class` := "ct-input ct-input-large"
  ).render


  private val special = Seq("!h1", "!h2", "!h3", "!h4", "!h5", "!h6", "!heading", "!code")
  private val specialDesc = s"Special commands: ${special.mkString(" ")}"


  override protected def headerSize: Int = 1

  override protected val list = div(
    flex := "1 1 auto",
    width := "100%",
    height := "calc(100% - 57px)",
    `class` := "ct-scroll ct-document-style ct-panel",
    div(`class` := "ct-desc", fontSize := "15px", paddingLeft := "10px", paddingRight := "10px", paddingBottom := "8px", specialDesc)
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
    val tt = term.split("\\s").toSeq.filter(complexTerm)
    if (tt.isEmpty) {
      Seq.empty
    } else {
      val raw = (tt.toSet -- special).map(a => model.data.Unicode(a.toLowerCase())).toSeq
      val isHeading = tt.contains("!heading")
      val reqireHeadingLevel =
        if (tt.contains("!h1")) {
          1
        } else if (tt.contains("!h2")) {
          2
        } else if (tt.contains("!h3")) {
          3
        } else if (tt.contains("!h4")) {
          4
        } else if (tt.contains("!h5")) {
          5
        } else if (tt.contains("!h6")) {
          6
        } else {
          -1
        }
      val requireCode = tt.contains("!code")
      client.state.quickSearch(raw, isHeading, reqireHeadingLevel, requireCode, delimitationGraphemes, opt).map(a => (a, client.state.node(a).uuid))
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
