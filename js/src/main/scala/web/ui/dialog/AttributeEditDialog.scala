package web.ui.dialog

import command.Key
import model.data.{SpecialChar, Text, Unicode}
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw._
import scalatags.JsDom
import scalatags.JsDom.all._
import view.{RichEditInterface, SourceEditInterface}
import web.view._
import web.ui._
import model.data.SpecialChar

object AttributeEditDialog {
  abstract class Anchor(val editor: RichEditInterface) extends OverlayAnchor {
  }
}
class AttributeEditDialog(protected val layer: OverlayLayer) extends MountedOverlay[AttributeEditDialog.Anchor] {



  override def focus(): Unit = {
    dom.children(1).asInstanceOf[Input].focus()
  }

  dom = div(
    cls := "ct-card unselectable",
    padding := "6px"
  ).render

  private var start: Seq[Unicode] = null
  private var attributes: Seq[SpecialChar] = null

  def titleOf(c: model.data.SpecialChar): String =
    if (c == model.data.ClassAttribute) "CSS classes"
    else if (c == model.data.TitleAttribute) "Title"
    else if (c == model.data.UrlAttribute) "URL"
    else "[WHAT??]"

  def show(anchor: AttributeEditDialog.Anchor, text: Text.Delimited): Unit = {
    start = text.attributeValues
    attributes = text.attributes
    text.attributes.zipWithIndex.foreach(c => {
      dom.appendChild(span(titleOf(c._1), cls := "ct-input-label").render)
      val ip = input(
        width := "100%",
        cls := "ct-input text-selectable",

      ).render
      ip.asInstanceOf[Input].value = start(c._2).str
      ip.addEventListener("keydown", (ev: KeyboardEvent) => {
        KeyMap.get(ev.key) match {
          case Some(Key.Escape) =>
            ev.preventDefault()
            dismiss()
          case Some(Key.Tab) =>
            ev.preventDefault()
            dismiss() // TODO fix this
          case Some(Key.Enter) =>
            ev.preventDefault()
            dismiss()
          case _ =>
        }
      })
      dom.appendChild(ip)
    })
    super.show(anchor)
  }


  override def show(anchor: AttributeEditDialog.Anchor): Unit = {
    throw new IllegalStateException("Call another one!")
  }

  override protected def onDismiss(): Unit = {
    val vals = start.zipWithIndex.map { pair =>
      val nv = Unicode(dom.childNodes(pair._2 * 2 + 1).asInstanceOf[Input].value)
      if (nv != pair._1) {
        Some(nv)
      } else {
        None
      }
    }
    if (!vals.forall(_.isEmpty)) {
      anchor.editor.onAttributeModified(attributes, vals)
    }
    removeAllChild(dom)
    anchor.editor.onExitSubMode()
    super.onDismiss()
  }
}
