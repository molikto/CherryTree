package web.view

import command.Key
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import util.Rect
import client.Client
import model.data.Unicode

object UrlAttributeEditDialog {
  trait Anchor extends OverlayAnchor {
    def update(url: Unicode, title: Unicode): Unit
  }
}
class UrlAttributeEditDialog(val client: Client, protected val layer: OverlayLayer) extends MountedOverlay[UrlAttributeEditDialog.Anchor] {

  private val urlInput = input(
    width := "100%",
    `class` := "ct-input text-selectable"
  ).render

  private val titleInput = input(
    width := "100%",
    `class` := "ct-input text-selectable"
  ).render


  override def focus(): Unit = {
    urlInput.focus()
  }

  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    `class` := "ct-card unselectable",
    padding := "6px",
    display := "none",
    span("URL", `class` := "ct-input-label"),
    div(width := "100%", urlInput),
    span("title", `class` := "ct-input-label", paddingTop := "12px"),
    div(width := "100%", titleInput),
  ).render

  private var urlStart: String = null
  private var titleStart: String = null

  def show(anchor: UrlAttributeEditDialog.Anchor, url: String, title: String): Unit = {
    urlStart = url
    titleStart = title
    urlInput.value = url
    titleInput.value = title
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
      anchor.update(Unicode(urlInput.value), Unicode(titleInput.value))
    }
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
