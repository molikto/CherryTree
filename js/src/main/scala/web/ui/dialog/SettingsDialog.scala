package web.ui.dialog

import java.util.UUID

import client.Client
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import settings.Settings
import web.ui.doc.{DocFramer, EditorView}
import web.view.{CoveringOverlay, Overlay, OverlayLayer, OverlayT, UnselectableView}


class SettingsDialog(val client: Client,
  override val layer: OverlayLayer,
  val coveringElement: HTMLElement
) extends OverlayT[Unit]
  with UnselectableView with Settings {


  override def show(t: Unit): Unit = {
    CoveringOverlay.show(layer, dom, coveringElement)
    super.show(t)
  }

  def checkboxOf(id0: String, label0: String, checked0: Boolean, change: Boolean => Unit): HTMLElement = {
    div(cls := "form-check",
      input(`type` := "checkbox", cls := "form-check-input", id := id0, checked := checked0, onchange := ((ev: UIEvent) => {
        change(ev.target.asInstanceOf[Input].checked)
      })),
      label(cls := "form-check-label", `for` := id0, label0)
    ).render
  }



  dom = div(
    cls := "ct-card",
    display := "flex",
    flexDirection := "column",
    paddingLeft := "20px",
    paddingRight := "20px",
    paddingTop := "16px",
    paddingBottom := "16px",
    fontSize := "1.15rem",
    div(
      width := "100%",
      height := "100%",
      cls := "ct-scroll",
      checkboxOf("enable-modal", "Enable modal mode", enableModal, ev => Unit)
    )
  ).render




}
