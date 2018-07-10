package web

import org.scalajs.dom
import org.scalajs.dom.{Node, html}


package object view {

  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]

  def removeAllChild(a: Node): Unit = {
    for (i <- 0 until a.childNodes.length) a.removeChild(a.childNodes.item(i))
  }
  def onlyChild(a: Node, b: Node) : Unit = {
    removeAllChild(a)
    a.appendChild(b)
  }

}
