package web.ui.dialog

import client.Client
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import settings.Settings
import web.view.{CoveringOverlay, OverlayLayer, UnselectableView}
import web.ui.doc.{DocFramer, EditorView}
import web.view._


class QuickSearchDialog(val client: Client,
  override val layer: OverlayLayer,
  val coveringElement: HTMLElement
) extends StaticFilteringView[Boolean, (model.cursor.Node, String)]
  with UnselectableView with Settings with DocFramer {


  override def show(t: Boolean): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    super.show(t)
  }

  override protected val search = input(
    width := "100%",
    `class` := "ct-input ct-input-large"
  ).render


  private val special = Seq("!h1", "!h2", "!h3", "!h4", "!h5", "!h6", "!heading", "!code", "!latexmacro")
  private val marks = delimitationGraphemes.values.toSet[model.data.Unicode].toSeq.map(_.str)
  private val specialDesc = s"Special commands: ${(special ++ marks).mkString(" ")}"


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


  private def complexTerm(str: String): Boolean = {
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
      val islm = tt.contains("!latexmacro")
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
      client.state.quickSearch(raw, islm, isHeading, reqireHeadingLevel, requireCode, delimitationGraphemes, opt).map(a => (a, client.state.node(a).uuid))
    }
  }


  override def onKeyDown(ev: KeyboardEvent): Boolean = {
    val key = Seq(EditorView.extractKey(ev))
    if (client.miscCommands.quickSearchDocument.keys.contains(key) && opt) {
      opt = false
      update()
      true
    } else if (client.miscCommands.quickSearchViewport.keys.contains(key) && !opt) {
      opt = true
      update()
      true
    } else {
      false
    }
  }

  override val docFramerIsSmall: Int = 1

  override def renderItem(t: (model.cursor.Node, String), index: Int): HTMLElement = {
    val node = client.state.node(t._1)
    val ct = contentViewAndHold(node)
    ct.classList.add("ct-menu-item")
    ct
  }

  override def onSelected(t: (model.cursor.Node, String)): Unit = {
    var n = t._1
    if (client.state.node(n).uuid != t._2) {
      val find = client.state.lookup(t._2)
      if (find.isDefined) {
        n = find.get
      } else {
        return
      }
    }
    client.localChange(client.state.goTo(n, client))
  }
}
