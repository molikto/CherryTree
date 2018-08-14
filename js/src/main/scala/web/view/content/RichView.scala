package web.view.content

import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view._

import scala.scalajs.js

class RichView(protected var rich: model.data.Rich) extends ContentView[model.data.Content.Rich, operation.Content.Rich] {


  protected val evilChar = "\u200B"
  /**
    *
    * state
    *
    */
  protected def isEmpty = rich.isEmpty

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



  protected def initDom(): Unit = {
    if (dom.childNodes.length == 0) {
      dom.appendChild(rec(rich.text).render)
      if (isEmpty) initEmptyContent()
    } else {
      throw new IllegalStateException("...")
    }
  }

  initDom()

  protected def clearDom(): Unit = {
    removeAllChild(dom)
  }


  protected def removeEmptyContent(): Unit = {
    removeAllChild(dom)
  }

  protected def initEmptyContent(): Unit = {
    removeAllChild(dom)
    dom.appendChild(span(EmptyStr, color := theme.disalbedInfo).render)
  }

  private def cg(a: String, extraClass: String = "") = span(`class` := "ct-cg " + extraClass,
    contenteditable := "false", a)

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
      case Text.Link(t, b, c) =>
        val tt: String = if (c.isEmpty) b.str else s"${c.str}\n${b.str}"
        span(
          title := tt,
          cg("["),
          span(`class` := "ct-link", rec(t)),
          cg("]")
        )
      case Text.Image(b, c) =>
        img(`class` := "ct-image", src := b.str, title := c.str)
      case Text.LaTeX(c) =>
        val a = span().render
        try {
          KaTeX.render(c.str, a)
        } catch {
          case a: Throwable => a.printStackTrace()
        }
        span(`class` := "ct-latex",
          contenteditable := false,
          span(evilChar), // don't fuck with my cursor!!!
          a,
          span(evilChar)
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

  protected def domAt(a: Seq[Int]): Node = domAt(dom, a)

  protected var insertEmptyTextNode: raw.Text = null

  protected def domAt(parent: Node, a: Seq[Int]): Node = {
    if (a.isEmpty) {
      parent
    } else {
      var c: Node = null
      val childArray = domChildArray(parent)
      if (insertEmptyTextNode == null || insertEmptyTextNode.parentNode != childArray) {
        c = childArray.childNodes(a.head)
      } else {
        c = childArray.firstChild
        if (c == insertEmptyTextNode) c = c.nextSibling
        var i = 0
        while (i < a.head) {
          c = c.nextSibling
          if (c != insertEmptyTextNode) {
            i += 1
          }
        }
      }
      domAt(c, a.tail)
    }
  }

  protected def domChildArray(parent: Node): Node = {
    if (parent.isInstanceOf[HTMLSpanElement]) {
      parent.childNodes(1)
    } else {
      parent
    }
  }

  protected def domCodeText(parent: Node): Node = {
    parent.childNodes(1).childNodes(0)
  }


  protected def nonEmptySelectionToDomRange(range: IntRange): (Range, HTMLSpanElement) = {
    assert(!range.isEmpty)
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
      val codeText = domCodeText(domAt(ss.nodeCursor))
      val ast = ss.text.asCoded
      val sss = ast.content.toStringPosition(ss.asInstanceOf[Atom.CodedGrapheme].unicodeIndex)
      val eee = ast.content.toStringPosition(ee.asInstanceOf[Atom.CodedGrapheme].unicodeIndex + ee.asInstanceOf[Atom.CodedGrapheme].size)
      (createRange(codeText, sss, codeText, eee), null)
    } else if (range.size == 1 &&
      ss.special) {
      val isStart = ss.delimitationStart
      val span = domAt(ss.nodeCursor).asInstanceOf[HTMLSpanElement]
      val (s, e) = if (isStart) (0, 1) else (2, 3)
      (createRange(span, s, span, e), span) // change this might causes problems when focus out then focus in...
    } else {
      val start = ss match {
        case grapheme: Atom.PlainGrapheme =>
          val text = domAt(grapheme.nodeCursor)
          val s = grapheme.text.unicode.toStringPosition(grapheme.unicodeIndex)
          (text, s)
        case aa =>
          assert(aa.delimitationStart || aa.isInstanceOf[Atom.Marked])
          val node = domChildArray(domAt(aa.nodeCursor.dropRight(1)))
          (node, aa.nodeCursor.last)
      }

      val end = ss match {
        case grapheme: Atom.PlainGrapheme =>
          val text = domAt(grapheme.nodeCursor)
          val s = grapheme.text.unicode.toStringPosition(grapheme.unicodeIndex + grapheme.size)
          (text, s)
        case aa =>
          assert(aa.delimitationEnd || aa.isInstanceOf[Atom.Marked])
          val node = domChildArray(domAt(aa.nodeCursor.dropRight(1)))
          (node, aa.nodeCursor.last + 1)
      }
      (createRange(start._1, start._2, end._1, end._2), null)
    }
  }



  /**
    *
    *
    * events
    *
    */


  override def updateContent(data: model.data.Content.Rich, c: operation.Content.Rich, viewUpdated: Boolean): Unit = {
    rich = data.content
    if (!viewUpdated) {
      clearDom()
      initDom()
    }
  }

}
