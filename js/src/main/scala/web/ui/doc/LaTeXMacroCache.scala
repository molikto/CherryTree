package web.ui.doc

import doc.DocState
import model.data.{Content, LaTeXMacro, Node}
import model.operation

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import web.ui._
import web.ui.content._

import scala.scalajs.js.|

class LaTeXMacroException(message: String, cause: Throwable = null) extends Exception(message, cause)

// LATER this should be a field in DocState... shit happens all the time
object LaTeXMacroCache {
  val instance: LaTeXMacroCache = new LaTeXMacroCache()

  def renderLaTeX(a: HTMLElement, unicode: String, index: Int, display: Boolean = false): Unit = {
    if (instance.dirty) {
      return
    }
    if (unicode.isEmpty) {
      a.insertBefore(warningInline("empty LaTeX").render, a.childNodes(index))
    } else {
      val b = span(
        `class` := "ct-latex"
      ).render
      try {
        instance.opts.asInstanceOf[js.Dynamic].displayMode = display
        KaTeX.render(unicode, b, instance.opts)
        a.insertBefore(b, a.childNodes(index))
      } catch {
        case err: Throwable =>
          val error = errorInline("LaTeX error", err).render
          error.title = err.getMessage
          a.insertBefore(error, a.childNodes(index))
      }
    }
  }
}

class LaTeXMacroCache() {

  def active(): Unit = {
   // LaTeXMacroCache.instance = this
  }
  // LATER now we are single instance! who cares!
  def inactive(): Unit = {
    //LaTeXMacroCache.instance = null
  }

  private var previous: Seq[String] = null
  private var dirt: Boolean = true

  def update(node: DocState): Unit = {
    val mm = node.node.macros
    if (previous != mm) {
      previous = mm
      dirt = true
    }
  }


  def dirty: Boolean = dirt

  def rebuildAndMarkNoDirty(): Unit = {
    target = js.Dynamic.literal(
      throwOnError = false
    )
    val fakeEle = span().render
    for (p <- previous) {
      try {
        KaTeX.render(p, fakeEle, js.Dynamic.literal(macros = target))
      } catch {
        case e: Throwable =>
          e.printStackTrace()
      }
    }
    opts = js.Dynamic.literal(
      throwOnError = false,
      macros = target
    )
    dirt = false
  }


  private var target = js.Dynamic.literal()

  private var opts: js.Object = target
//    js.Dynamic.literal(macros =
//      js.Dynamic.newInstance(window.asInstanceOf[js.Dynamic].Proxy)(
//        target, js.Dynamic.literal(
//          set = ((target: js.Object, key: String | js.Symbol, value: js.UndefOr[String]) => {
//            throw new LaTeXMacroException(s"Not allowed to set value $key $value")
//          }): js.Function3[js.Object, String | js.Symbol, js.UndefOr[String], Unit],
//          deleteProperty = ((target: js.Object, key: String | js.Symbol) => {
//            throw new LaTeXMacroException(s"Not allowed to delete property $key")
//          }): js.Function2[js.Object, String | js.Symbol, js.UndefOr[String]],
//          defineProperty = ((target: js.Object, key: String | js.Symbol, descriptor: js.Dynamic) => {
//            throw new LaTeXMacroException(s"Not allowed to define property $key")
//            Unit
//          }): js.Function3[js.Object, String | js.Symbol, js.Dynamic, Unit],
//        )
//    )
//)
}
