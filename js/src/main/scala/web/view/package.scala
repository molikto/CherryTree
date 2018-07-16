package web

import command.Key
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLDivElement
import org.scalajs.dom.{Event, Node, html}

import scala.scalajs.js


package object view {
  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]

  def removeAllChild(a: Node): Unit = {
    for (i <- 0 until a.childNodes.length) a.removeChild(a.childNodes(i))
  }
  def onlyChild(a: Node, b: Node) : Unit = {
    removeAllChild(a)
    a.appendChild(b)
  }


  // https://developer.mozilla.org/zh-CN/docs/Web/API/KeyboardEvent/key/Key_Values
  val KeyMap: Map[String, Key.V] = {
    import Key._
    Map(
      "Home" -> Home,
      "End" -> End,
      "ArrowLeft" -> Left,
      "ArrowRight" -> Right,
      "ArrowUp" -> Up,
      "ArrowDown" -> Down,
      "Enter" -> Enter,
      "PageDown" -> PageDown,
      "PageUp" -> PageUp,
      "Backspace" -> Backspace,
      "Tab" -> Tab,
      "Escape" -> Escape,
      "Shift" -> Shift,
      "Meta" -> Meta,
      "Control" -> Control,
      "Alt" -> Alt
    )
  }
}
