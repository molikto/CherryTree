package web.view

import org.scalajs.dom.Event

class UnselectableView extends View {


  override def onAttach(): Unit = {
    dom.classList.add("unselectable")
    event("contextmenu", (e: Event) => preventDefault(e))
  }
}
