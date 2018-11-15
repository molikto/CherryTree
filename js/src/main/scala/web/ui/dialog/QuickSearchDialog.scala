package web.ui.dialog

import java.util.UUID

import client.Client
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import settings.Settings
import web.view.{CoveringOverlay, OverlayLayer, UnselectableView}
import web.ui.doc.{DocFramer, EditorView, LaTeXMacroCache}
import web.view._


class QuickSearchDialog(val client: Client,
  override val layer: OverlayLayer,
  val coveringElement: HTMLElement,
  override val latexMacroCache: LaTeXMacroCache
) extends StaticFilteringView[Boolean, (model.cursor.Node, UUID)]
  with UnselectableView with DocFramer {


  def showWithTag(t: model.data.Text.HashTag): Unit = {
    val terms = "#" + model.data.Text.toPlain(t.content)
    show(false)
    search.value = terms
  }


  override def show(t: Boolean): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    client.disableRemoteStateUpdate(true, "quick search")
    super.show(t)
  }



  override protected def onDismiss(): Unit = {
    super.onDismiss()
    client.disableRemoteStateUpdate(false, "quick search")
  }

  override protected val search = input(
    width := "100%",
    cls := "ct-input ct-input-large"
  ).render


  private val special = Seq("!h1", "!h2", "!h3", "!h4", "!h5", "!h6", "!heading", "!code", "!latexmacro")
  private val marks = client.delimitationGraphemes.values.toSet[model.data.Unicode].toSeq.map(_.str)
  private val specialDesc = s"Special commands: #hashtag ${(special ++ marks).mkString(" ")}"


  override protected def headerSize: Int = 1

  override protected val list = div(
    flex := "1 1 auto",
    width := "100%",
    height := "calc(100% - 57px)",
    cls := "ct-scroll ct-document-style ct-panel",
    div(cls := "ct-desc", fontSize := "15px", paddingLeft := "10px", paddingRight := "10px", paddingBottom := "8px", specialDesc)
  ).render

  dom = div(
    cls := "ct-card",
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


  override def data(term: String): Seq[(model.cursor.Node, UUID)] = {
    val tt = term.split("\\s").toSeq.filter(complexTerm)
    if (tt.isEmpty) {
      Seq.empty
    } else {
      val raw = (tt.toSet -- special).map(a => model.data.Unicode(a.toLowerCase())).toSet
      val hashes0 = raw.filter(_.startsWith("#"))
      val hashes = hashes0.map(_.drop(1)).filter(_.nonEmpty)
      val terms = raw -- hashes0
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
      client.state.quickSearch(terms, hashes, islm, isHeading, reqireHeadingLevel, requireCode, client.delimitationGraphemes, opt).map(a => (a, client.state.node(a).uuid))
    }
  }


  override def onKeyDown(ev: KeyboardEvent): Boolean = {
    val key = Seq(EditorView.extractKey(ev))
    if (client.commands.miscCommands.quickSearchDocument.keys.contains(key) && opt) {
      opt = false
      update()
      true
    } else if (client.commands.miscCommands.quickSearchViewport.keys.contains(key) && !opt) {
      opt = true
      update()
      true
    } else {
      false
    }
  }

  override val docFramerIsSmall: Int = 1

  override def renderItem(t: (model.cursor.Node, UUID), index: Int): HTMLElement = {
    val node = client.state.node(t._1)
    val ct = contentViewAndHold(node)
    ct.classList.add("ct-menu-item")
    ct
  }

  override def onSelected(t: (model.cursor.Node, UUID)): Unit = {
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
