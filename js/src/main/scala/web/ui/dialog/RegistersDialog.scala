package web.ui.dialog

import client.Client
import command.Command
import model.data.Node.ContentType
import model.data.Rich
import org.scalajs.dom.raw._
import register.Registerable
import scalatags.JsDom.all._
import web.ui.doc.{DocFramer, DocumentView, LaTeXMacroCache}
import web.view._

class RegistersDialog(val client: Client, protected val layer: OverlayLayer,
  override val latexMacroCache: LaTeXMacroCache) extends
  StaticFilteringView[OverlayAnchor, (Int, Option[Registerable])]
    with MountedOverlay[OverlayAnchor] with DocFramer
{


  focusOutDismiss = true

  override val docFramerIsSmall: Int = 2

  override protected val search = input(
    width := "100%",
    cls := "ct-input"
  ).render

  override protected val list = div(
    width := "100%",
    maxHeight := "280px",
    minHeight := "0px",
    overflowY := "scroll",
    overflowX := "hidden",
    cls := "ct-scroll ct-panel"
  ).render


  dom = div(
    cls := "ct-card",
    div(width := "100%", padding := "6px", search),
    width := "480px",
    list
  ).render

  private var regs: Seq[(Int, Option[Registerable])] = Seq.empty

  override def show(anchor: OverlayAnchor): Unit = {
    regs = client.registerables.sortBy(a => if (a._2.isDefined) 0 else 1)
    super.show(anchor)
    if (list.childNodes.length == headerSize) update()
  }

  override def data(term: String) = if (term.isEmpty) regs else regs.filter(_._1 == term.codePointAt(0))

  override def renderItem(t:  (Int, Option[Registerable]), index: Int): HTMLElement = {
    div(
      display := "flex",
      flexDirection := "row",
      alignItems := "center",
      cls := "ct-menu-item ",
      paddingLeft := "5px",
      tag("kbd")(cls := "ct-kbd-small", Character.toString(t._1.toChar)),
      div(
        marginLeft := "10px",
        t._2.map {
          case Registerable.Unicode(u) => code(cls := "ct-c-code", u.str)
          case Registerable.Text(u) => contentViewCreate(model.data.Content.Rich(Rich(u)), None, false) : Frag
          case Registerable.Node(a, _) =>
            if (a.isEmpty) "": Frag
            else {
              val count = a.map(_.count).sum
              if (count == 1) {
                contentViewCreate(a.head.content, a.head.contentType): Frag
              } else {
                div(
                  s"$count nodes",
                  contentViewCreate(a.head.content, a.head.contentType)
                )
              }
            }
        }.getOrElse("": Frag)
      )
    ).render
  }

  override def onSelected(t:  (Int, Option[Registerable])): Unit = {
    client.setRegister(t._1)
  }
}
