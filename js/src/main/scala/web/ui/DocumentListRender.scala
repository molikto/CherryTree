package web.ui

import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._


@JSExportTopLevel("DocumentListRender")
class DocumentListRender(rootView: HTMLElement) {

  rootView.appendChild(p("what").render)
}
