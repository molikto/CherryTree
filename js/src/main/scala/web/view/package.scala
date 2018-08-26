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


  def contains(p: ClientRect, c: ClientRect): Boolean = {
    p.top <= c.top && p.left <= c.left && p.right >= c.right && p.bottom >= c.bottom
  }

  def scrollRectInToViewIfNotVisible(
    c: ClientRect,
    scroll: HTMLElement,
    topCut: Int = 0,
    bottomCut: Int = 0,
    toTopCut: Int = 0,
    toBottomCut: Int = 0
  ): Unit = {
    val p = scroll.getBoundingClientRect()
    if (c.top < p.top + p.height * topCut / 100) {
      if (model.debug_scroll) {
        window.console.log(s"scrolling to top", c, p, topCut, bottomCut, toTopCut, toBottomCut)
      }
      scroll.scrollTop = scroll.scrollTop - (p.top + p.height * toTopCut / 100 - c.top)
    } else if (c.bottom > p.bottom - p.height * bottomCut / 100) {
      if (model.debug_scroll) {
        window.console.log(s"scrolling to bottom", c, p, topCut, bottomCut, toTopCut, toBottomCut)
      }
      scroll.scrollTop = scroll.scrollTop + ((c.bottom - p.bottom + p.height * toBottomCut / 100) min (c.top - p.top - p.height * toTopCut / 100))
    }
  }

  def scrollInToViewIfNotVisible(
    a: HTMLElement,
    scroll: HTMLElement,
    topCut: Int = 0,
    bottomCut: Int = 0,
    toTopCut: Int = 0,
    toBottomCut: Int = 0): Unit = {
    scrollRectInToViewIfNotVisible(a.getBoundingClientRect(), scroll, topCut, bottomCut, toTopCut, toBottomCut)
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
