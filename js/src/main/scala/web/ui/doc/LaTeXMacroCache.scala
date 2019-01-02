package web.ui.doc

import doc.DocState
import scala.scalajs.js
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import web.ui._
import web.ui.content._


class LaTeXMacroException(message: String, cause: Throwable = null) extends Exception(message, cause)


object LaTeXMacroCache {
  val empty: LaTeXMacroCache = {
    val a = new LaTeXMacroCache()
    a.dirt = false
    a
  }
}

class LaTeXMacroCache() {

  def renderLaTeX(a: HTMLElement, unicode: String, index: Int, display: Boolean = false): Unit = {
    if (dirty) {
      return
    }
    if (unicode.isEmpty) {
      a.insertBefore(warningInline("empty LaTeX").render, a.childNodes(index))
    } else {
      val b = span(
        cls := "ct-latex"
      ).render
      try {
        opts.asInstanceOf[js.Dynamic].displayMode = display
        KaTeX.render(unicode, b, opts)
        a.insertBefore(b, a.childNodes(index))
      } catch {
        case err: Throwable =>
          val error = errorInline("LaTeX error", err).render
          error.title = err.getMessage
          a.insertBefore(error, a.childNodes(index))
      }
    }
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
