package web.view

import command.Key
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import util.Rect
import client.Client

class UrlAttributeEditDialog(val client: Client, protected val layer: OverlayLayer) extends MountedOverlay {

  private val urlInput = input(
    width := "100%",
    `class` := "ct-input"
  ).render


  override def focus(): Unit = {
    urlInput.focus()
  }

  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    `class` := "ct-card",
    display := "none",
    div(width := "100%", padding := "6px", urlInput)
  ).render


  override def show(anchor: OverlayAnchor): Unit = {
    urlInput.value = ""
    super.show(anchor)
  }



  event(urlInput, "keydown", (ev: KeyboardEvent) => {
    KeyMap.get(ev.key) match {
      case Some(Key.Escape) =>
        ev.preventDefault()
        dismiss()
      case Some(Key.Enter) =>
        ev.preventDefault()
      case Some(Key.Down) =>
        ev.preventDefault()
      case Some(Key.Up) =>
        ev.preventDefault()
      case _ =>
    }
  })
}
