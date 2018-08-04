package web.view

import command.Key
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import util.Rect
import client.Client

class AttributeEditDialog(val client: Client, protected val layer: OverlayLayer) extends Overlay {

  private val search = input(
    width := "100%",
    `class` := "ct-input"
  ).render


  override def focus(): Unit = {
    search.focus()
  }

  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    `class` := "ct-card",
    display := "none",
    div(width := "100%", padding := "6px", search)
  ).render



  def showAt(rect: Rect): Unit = {
    setDomAttributeBy(rect)
    search.textContent = ""
    showOverlay()
  }





  override protected def onDismiss(): Unit = {
    super.onDismiss()
  }

  event(search, "keydown", (ev: KeyboardEvent) => {
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

  event(dom, "focusout", (ev: FocusEvent) => {
    dismiss()
  })
}
