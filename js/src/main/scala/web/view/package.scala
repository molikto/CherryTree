package web

import command.Key
import command.Key.KeySeq
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLDivElement, HTMLElement}
import org.scalajs.dom._
import _root_.util.Rect
import scalatags.jsdom.Frag

import scala.scalajs.js


package object view {



  val EmptyStr = "∅"
  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]

  def elementParent(a: Node): HTMLElement = {
    a match {
      case element: HTMLElement => element
      case _ => elementParent(a.parentNode)
    }
  }

  def removeAllChild(a: Element): Unit = {
    a.innerHTML = ""
  }
  def onlyChild(a: HTMLElement, b: Node) : Unit = {
    removeAllChild(a)
    a.appendChild(b)
  }

  def removeFromParent(extraNode: Node): Unit = {
    if (extraNode != null && extraNode.parentNode != null) extraNode.parentNode.removeChild(extraNode)
  }

  def indexOf(c: Node, extraNode: Node = null): Int = indexOf(c.parentNode, c, extraNode)

  def indexOf(p: Node, c: Node, extraNode: Node): Int = {
    var j = 0
    var ch = p.firstChild
    while (ch != null) {
      if (ch == extraNode) {
      } else if (ch == c) {
        return j
      } else {
        j += 1
      }
      ch = ch.nextSibling
    }
    -1
  }


  def removeAllClass(cs: HTMLElement) ={
    while (cs.classList.length > 0) {
      cs.classList.remove(cs.classList.item(cs.classList.length - 1))
    }
  }

  def exitsClassPrefix(element: HTMLElement, function: String): Boolean = {
    val cn = element.className
    if (cn.isEmpty) {
      false
    } else {
      cn.startsWith(function) || cn.contains(" " + function)
    }
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

  def toRect(rect: ClientRect): Rect = {
    Rect(rect.left, rect.top, rect.width, rect.height)
  }

  implicit def viewToFrag(v: View): Frag = {
    new Frag {
      override def render: Node = v.dom
      override def applyTo(b: dom.Element) = v.attachToNode(b)
    }
  }

}
