package web.view.content

import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw.{CompositionEvent, Element, ErrorEvent, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view._
import web.view.content.ContentViewEditor.General

import scala.scalajs.js

object RichView {
  val EvilChar = "\u200B"
}
class RichView(private[content] var rich: model.data.Rich) extends ContentView[model.data.Content.Rich, operation.Content.Rich] {
  override def contentData = model.data.Content.Rich(rich)
  override def contentData_=(c: model.data.Content.Rich): Unit = {
    rich = c.content
  }
  
  


  import RichView._


  override def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General =
    new RichViewEditor(documentView, controller, this).asInstanceOf[ContentViewEditor.General]

  /**
    *
    * state
    *
    */
  private[content] def isEmpty = rich.isEmpty

  /**
    *
    *
    *
    * dom creation and query & modifictaion methods
    *
    *
    *
    */

  dom = p(`class` := "ct-rich").render

  private[content] def root: HTMLElement = dom

  private[content] var previousMode = if (isEmpty) -2 else -1

  private[content] def setPreviousModeToEmpty(): Unit = {
    previousMode = if (isEmpty) -2 else -1
  }


  private[content] def initDom(): Unit = {
    if (root.childNodes.length == 0) {
      root.appendChild(rec(rich.text).render)
      if (isEmpty) initEmptyContent()
    } else {
      throw new IllegalStateException("...")
    }
  }

  initDom()

  private[content] def clearDom(): Unit = {
    setPreviousModeToEmpty()
    removeAllChild(root)
  }


  private[content] def removeEmptyContent(): Unit = {
    removeAllChild(root)
  }

  private[content] def initEmptyContent(): Unit = {
    removeAllChild(root)
    root.appendChild(span(`class` := "ct-hint-color", EmptyStr).render)
  }

  private def cg(a: String, extraClass: String = "") = span(`class` := "ct-cg " + extraClass,
    contenteditable := "false", a)

  val onImageError: js.Function1[ErrorEvent, _] = (e: ErrorEvent) => {
    val el = e.target.asInstanceOf[HTMLElement]
    window.console.log("image loading error")
    el.style.width = "12px"
    el.style.height= "12px"
  }

  private def rec(seq: Seq[model.data.Text]): Seq[Frag] = {
    seq.map {
      case Text.Emphasis(c) => span(
        cg("*"),
        em(`class` := "ct-em", rec(c)),
        cg("*")
      )
      case Text.Strong(c) => span(
        cg("*", "ct-cg-shadow"),
        strong(`class` := "ct-strong", rec(c)),
        cg("*", "ct-cg-shadow")
      )
      case Text.StrikeThrough(c) => span(
        cg("~"),
        del(`class` := "ct-del", rec(c)),
        cg("~")
      )
      case l@Text.Link(t, b, c) =>
        val tt: String = if (c.isEmpty) b.str else s"${c.str}\n${b.str}"
        span(
          title := tt,
          cg("["),
          span(`class` := (if (l.isNodeRef) "ct-link-node" else "ct-link"), rec(t)),
          cg("]")
        )
      case Text.Image(b, c) =>
        val sp = span().render
        if (b.isEmpty) {
          sp.appendChild(warningInline("empty image").render)
        } else {
          sp.appendChild(img(`class` := "ct-image", title := c.str, src := b.str, onerror := {e: Event => {
            removeAllChild(sp)
            sp.appendChild(errorInline("image error").render)
          }}).render)
        }
        sp: Frag
      case Text.HTML(c) =>
        val a = span(contenteditable := "false",
          display := "inline-block",
          span(EvilChar, contenteditable := false) // don't fuck with my cursor!!!
        ).render
        if (c.isBlank) {
          a.className = ""
          a.appendChild(warningInline("empty inline HTML").render)
        } else {
          try {
            a.className = "ct-inline-html"
            val b = span().render
            b.innerHTML = c.str
            a.appendChild(b)
          } catch {
            case err: Throwable =>
              a.className = ""
              a.appendChild(errorInline("inline HTML error", err).render)
          }
        }
        a.appendChild(span(EvilChar, contenteditable := false).render)
        a: Frag
      case Text.LaTeX(c) =>
        val a = span(display := "inline-block").render
        if (c.isBlank) {
          a.appendChild(warningInline("empty LaTeX").render)
        } else {
          try {
            KaTeX.render(c.str, a)
          } catch {
            case err: Throwable =>
              a.appendChild(errorInline("LaTeX error", err).render)
          }
        }
        span(`class` := "ct-latex",
          contenteditable := false,
          display := "inline-block",
          boxSizing := "border-box",
          span(EvilChar), // don't fuck with my cursor!!!
          a,
          span(EvilChar)
        )
      case Text.Code(c) =>
        span(
          cg("`"),
          code(`class` := "ct-code", c.str),
          cg("`")
        )
      case Text.Plain(c) => stringFrag(c.str)
    }
  }

  private[content] def cursorOf(t: raw.Text): model.cursor.Node = {
    def rec(t: Node): Seq[Int] = {
      if (t.parentNode == root) {
        Seq(indexOf(t, extraNode))
      } else {
        rec(t.parentNode.parentNode) :+ indexOf(t, extraNode)
      }
    }
    rec(t)
  }

  private[content] def nodeAt(a: Seq[Int]): Node = nodeAt(root, a)

  private[content] var extraNode: raw.Text = null

  private[content] def nodeAt(parent: Node, a: Seq[Int]): Node = {
    if (a.isEmpty) {
      parent
    } else {
      var c: Node = null
      val childArray = nodeChildArray(parent)
      if (extraNode == null || extraNode.parentNode != childArray) {
        c = childArray.childNodes(a.head)
      } else {
        c = childArray.firstChild
        if (c == extraNode) c = c.nextSibling
        var i = 0
        while (i < a.head) {
          c = c.nextSibling
          if (c != extraNode) {
            i += 1
          }
        }
      }
      nodeAt(c, a.tail)
    }
  }

  private[content] def nodeChildArray(parent: Node): Node = {
    if (parent.isInstanceOf[HTMLSpanElement]) {
      parent.childNodes(1)
    } else {
      parent
    }
  }

  private[content] def nonEmptySelectionToDomRange(range: IntRange): (Range, HTMLSpanElement) = {
    assert(!range.isEmpty, "range is empty")
    def createRange(a: Node, b: Int, c: Node, d: Int): Range = {
      val rr = document.createRange()
      rr.setStart(a, b)
      rr.setEnd(c, d)
      rr
    }
    val ss = rich.after(range.start)
    val ee = rich.before(range.until)
    if (ss.isInstanceOf[Atom.CodedGrapheme] &&
      ee.isInstanceOf[Atom.CodedGrapheme] &&
      ss.nodeCursor == ee.nodeCursor &&
      ss.text.isCodedNonAtomic) {
      val codeText = nodeAt(ss.nodeCursor)
      val ast = ss.text.asCoded
      val sss = ast.content.toStringPosition(ss.asInstanceOf[Atom.CodedGrapheme].unicodeIndex)
      val eee = ast.content.toStringPosition(ee.asInstanceOf[Atom.CodedGrapheme].unicodeIndex + ee.asInstanceOf[Atom.CodedGrapheme].size)
      (createRange(codeText, sss, codeText, eee), null)
    } else if (ss.totalIndex == ee.totalIndex &&
      ss.special) {
      val isStart = ss.delimitationStart
      val span = nodeAt(ss.nodeCursor).asInstanceOf[HTMLSpanElement]
      val (s, e) = if (isStart) (0, 1) else (2, 3)
      (createRange(span, s, span, e), span) // change this might causes problems when focus out then focus in...
    } else {
      val start = ss match {
        case grapheme: Atom.PlainGrapheme =>
          val text = nodeAt(grapheme.nodeCursor)
          val s = grapheme.text.unicode.toStringPosition(grapheme.unicodeIndex)
          (text, s)
        case aa =>
          assert(aa.delimitationStart || aa.isInstanceOf[Atom.Marked], s"expecting a deli start $aa")
          val node = nodeChildArray(nodeAt(model.cursor.Node.parent(aa.nodeCursor)))
          (node, aa.nodeCursor.last)
      }

      val end = ee match {
        case grapheme: Atom.PlainGrapheme =>
          val text = nodeAt(grapheme.nodeCursor)
          val s = grapheme.text.unicode.toStringPosition(grapheme.unicodeIndex + grapheme.size)
          (text, s)
        case aa =>
          assert(aa.delimitationEnd || aa.isInstanceOf[Atom.Marked], s"expecting a deli end, $aa")
          val node = nodeChildArray(nodeAt(model.cursor.Node.parent(aa.nodeCursor)))
          (node, aa.nodeCursor.last + 1)
      }
      (createRange(start._1, start._2, end._1, end._2), null)
    }
  }



  override def updateContent(): Unit = {
    clearDom()
    initDom()
  }

  override def updateContent(data: model.data.Content.Rich, c: operation.Content.Rich, viewUpdated: Boolean): Unit = {
    rich = data.content
    if (!viewUpdated) {
      updateContent()
    }
  }

}
