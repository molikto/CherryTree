package web

import command.Key
import command.Key.KeySeq
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLDivElement, HTMLElement}
import org.scalajs.dom._

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

  def jsObject(a: js.Dynamic => Unit): js.Dynamic = {
    val k =
      js.Object().asInstanceOf[js.Dynamic]
    a(k)
    k
  }

  def renderKeySeq(k: KeySeq): String = {
    if (k.forall(a => !a.control && !a.meta && a.a.isInstanceOf[Key.Grapheme])) {
      k.map(_.a.asInstanceOf[Key.Grapheme]).mkString("")
    } else {
      k.mkString(" ")
    }
  }


  def svgSourceToBackgroundStr(svg: String): String = {
    val encoded = window.btoa(svg)
    "url(data:image/svg+xml;base64," + encoded + ")"
  }

  def scrollInToViewIfNotVisible(a: HTMLElement, scroll: HTMLElement) = {
    if (a.offsetTop >= scroll.scrollTop && a.offsetTop + a.clientHeight <= scroll.scrollTop + scroll.clientHeight) {
    } else {
      a.scrollIntoView(false)
    }
  }
}
