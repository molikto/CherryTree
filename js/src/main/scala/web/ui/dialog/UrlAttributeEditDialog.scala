package web.ui.dialog

import command.Key
import model.data.Unicode
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import view.{RichEditInterface, SourceEditInterface}
import web.view._
import web.ui._

object UrlAttributeEditDialog {
  abstract class Anchor(val editor: RichEditInterface) extends OverlayAnchor {
  }
}
class UrlAttributeEditDialog(protected val layer: OverlayLayer) extends MountedOverlay[UrlAttributeEditDialog.Anchor] {

  private val urlInput = input(
    width := "100%",
    cls := "ct-input text-selectable"
  ).render

  private val titleInput = input(
    width := "100%",
    cls := "ct-input text-selectable"
  ).render


  override def focus(): Unit = {
    urlInput.focus()
  }

  dom = div(
    cls := "ct-card unselectable",
    padding := "6px",
    span("URL", cls := "ct-input-label"),
    div(width := "100%", urlInput, marginBottom := "6px"),
    span("title", cls := "ct-input-label"),
    div(width := "100%", titleInput),
  ).render

  private var urlStart: String = null
  private var titleStart: String = null

  def show(anchor: UrlAttributeEditDialog.Anchor, url: Unicode, title: Unicode): Unit = {
    urlStart = url.str
    titleStart = title.str
    urlInput.value = url.str
    titleInput.value = title.str
    super.show(anchor)
  }


  override def show(anchor: UrlAttributeEditDialog.Anchor): Unit = {
    throw new IllegalStateException("Call another one!")
  }

  override protected def onDismiss(): Unit = {
    var url = urlInput.value
    if (url.isEmpty) url = urlStart
    val title = titleInput.value
    if (url != urlStart || title != titleStart) {
      anchor.editor.onAttributeModified(Unicode(urlInput.value), Unicode(titleInput.value))
    }
    anchor.editor.onExitSubMode()
    super.onDismiss()
  }




  event(titleInput, "keydown", (ev: KeyboardEvent) => {
    KeyMap.get(ev.key) match {
      case Some(Key.Escape) =>
        ev.preventDefault()
        dismiss()
      case Some(Key.Tab) =>
        ev.preventDefault()
        urlInput.focus()
      case Some(Key.Enter) =>
        ev.preventDefault()
        dismiss()
      case _ =>
    }
  })


  event(urlInput, "keydown", (ev: KeyboardEvent) => {
    KeyMap.get(ev.key) match {
      case Some(Key.Escape) =>
        ev.preventDefault()
        dismiss()
      case Some(Key.Enter) =>
        ev.preventDefault()
        dismiss()
      case _ =>
    }
  })

}
