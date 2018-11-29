package web.ui.dialog

import java.util.UUID

import client.{Client, LocalStorage}
import command.Key.KeySeq
import model.data.{SpecialChar, Unicode}
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import settings.{Settings, SettingsImpl, SpecialKeySettings}
import web.view.{CoveringOverlay, Overlay, OverlayLayer, OverlayT, UnselectableView}

import scala.collection.mutable.ArrayBuffer


class SettingsTemp(from: Settings) extends Settings {
  var enableModal: Boolean = from.enableModal

  var delimitationSettings: Seq[(SpecialChar.Delimitation, Unicode, Unicode)] = from.delimitationSettings
  var delimitationGraphemes: SpecialKeySettings = from.delimitationGraphemes
  var additionalKeyMaps: Map[String, Seq[KeySeq]] = from.additionalKeyMaps
  var removedDefaultKeyMaps: Map[String, Seq[KeySeq]] = from.removedDefaultKeyMaps

  override def disableDelmitationKeys: Set[SpecialChar.Delimitation] = from.disableDelmitationKeys
}
class SettingsDialog(val client: Client,
  override val layer: OverlayLayer,
  val onSettingsChangeRefresh: () => Unit,
  val coveringElement: HTMLElement
) extends OverlayT[Unit]
  with UnselectableView {


  var temp: SettingsTemp = null

  override def show(t: Unit): Unit = {
    temp = new SettingsTemp(client)
    CoveringOverlay.show(layer, dom, coveringElement)
    onShows.foreach(_.apply())
    super.show(t)
  }

  private val onShows = new ArrayBuffer[() => Unit]()

  def checkboxOf(id0: String, label0: String, checked0: () => Boolean, change: Boolean => Unit): HTMLElement = {
    val check = input(`type` := "checkbox", cls := "form-check-input", id := id0, onchange := ((ev: UIEvent) => {
      change(ev.target.asInstanceOf[Input].checked)
    })).render
    onShows.append(() => check.checked = checked0())
    div(cls := "form-check",
      check,
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
      checkboxOf("enable-modal", "Enable modal mode", () => temp.enableModal, ev => temp.enableModal = ev),
      button(`type` := "submit", cls := "btn btn-primary","Submit", onclick := ((ev: MouseEvent) => {
        if (client.changeSettings(temp)) {
          onSettingsChangeRefresh()
        }
        if (!destroyed) dismiss()
      })))
  ).render




}
