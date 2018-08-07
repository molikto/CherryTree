package web

import command.Key
import command.Key.KeySeq
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLDivElement, HTMLElement}
import org.scalajs.dom._
import util.Rect

import scala.scalajs.js


package object view {

  val EmptyStr = "∅"
  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]

  def removeAllChild(a: HTMLElement): Unit = {
    a.innerHTML = ""
  }
  def onlyChild(a: HTMLElement, b: Node) : Unit = {
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
      "Control" -> Ctrl,
      "Delete" -> Delete,
      "Alt" -> Alt
    )
  }

  val jQ: js.Dynamic = js.Dynamic.global.jQuery

  val CodeMirror: js.Dynamic = js.Dynamic.global.CodeMirror

  val KaTeX = window.asInstanceOf[js.Dynamic].katex

  def jsObject(a: js.Dynamic => Unit): js.Dynamic = {
    val k =
      js.Object().asInstanceOf[js.Dynamic]
    a(k)
    k
  }


  def svgSourceToBackgroundStr(svg: String): String = {
    val encoded = window.btoa(svg)
    "url(data:image/svg+xml;base64," + encoded + ")"
  }

  def contains(p: ClientRect, c: ClientRect): Boolean = {
    p.top <= c.top && p.left <= c.left && p.right >= c.right && p.bottom >= c.bottom
  }

  def scrollInToViewIfNotVisible(c: ClientRect, scroll: HTMLElement): Unit = {
    val p = scroll.getBoundingClientRect()
    if (c.top < p.top) {
      scroll.scrollTop = scroll.scrollTop - (p.top - c.top)
    } else if (c.bottom > p.bottom) {
      scroll.scrollTop = scroll.scrollTop + ((c.bottom - p.bottom) min (c.top - p.top))
    }
  }

  def scrollInToViewIfNotVisible(a: HTMLElement, scroll: HTMLElement): Unit = {
    scrollInToViewIfNotVisible(a.getBoundingClientRect(), scroll)
  }

  def toRect(rect: ClientRect): util.Rect = {
    Rect(rect.left, rect.top, rect.width, rect.height)
  }
}
