package web.view

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._

import scala.scalajs.js


/**
  * this is how we do lists.
  *
  * limitations
  *
  * 1. subclasses create the `dom` node, and SHOULD NOT add any children to it
  * 2. parent node width/height SHOULD NOT depends on children size logic
  * 3. children node width/height SHOULD only depends on parent width and it's own data and should only have auto positioning
  * 4. parent must be a positioned element, use relative
  * 5. parent node must starts with scrollTop = 0 when attached
  * 6. child node should not have 0 height
  *
  * metrics
  *
  * 1. parent size, scroll top
  * 2. children size
  *
  */
object IndefiniteView {
  trait DataSource[T] {
    def head: Option[T]
    def next(a: T): Option[T]
  }
}
trait IndefiniteView[T] extends View {


  private val phAbove = div(height := "0px").render
  private val phBelow = div(height := "0px").render
  private val viewBufferSize = 1000
  private val viewBufferSizeStyle = viewBufferSize + "px"

  protected val data: IndefiniteView.DataSource[T]


  def render(t: T): HTMLElement


  private var seenChilds = 0
  private var avHeight = 0.0
  private def callRender(i: T): HTMLElement = {
    val c = render(i)
    if (seenChilds == 0) {
      avHeight = c.offsetHeight
    } else {
      avHeight = (avHeight * seenChilds + c.offsetHeight) / (seenChilds + 1)
    }
    seenChilds += 1
    c
  }


  private val refreshOnAnimationFrame: js.Function1[UIEvent, Unit] = _ => {

  }

  override def onAttach(): Unit = {
//    super.onAttach()
//    dom.appendChild(phAbove)
//    assert(dom.scrollTop == 0)
//    var i = data.head.orNull
//    var lastHTML: HTMLElement = null
//    while (i != null && lastHTML.offsetTop + lastHTML.offsetHeight < dom.clientHeight) {
//      lastHTML = callRender(i)
//      dom.appendChild(lastHTML)
//      i = data.next(i).orNull
//    }
//    dom.appendChild(phBelow)
//    if (i != null) {
//      phBelow.style.height = viewBufferSizeStyle
//    }
//    dom.addEventListener("scroll", refreshOnAnimationFrame)
//    defer(_ => dom.removeEventListener("scroll", refreshOnAnimationFrame))
  }


}
