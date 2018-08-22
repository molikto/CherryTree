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
        em(`class` := "ct-c-em", rec(c)),
        cg("*")
      )
      case Text.Strong(c) => span(
        cg("*", "ct-cg-shadow"),
        strong(`class` := "ct-c-strong", rec(c)),
        cg("*", "ct-cg-shadow")
      )
      case Text.StrikeThrough(c) => span(
        cg("~"),
        del(`class` := "ct-c-del", rec(c)),
        cg("~")
      )
      case l@Text.Link(t, b, c) =>
        val tt: String = if (c.isEmpty) b.str else s"${c.str}\n${b.str}"
        span(
          title := tt,
          cg("["),
          span(`class` := (if (l.isNodeRef) "ct-c-link-node" else "ct-c-link"), rec(t)),
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
          code(`class` := "ct-c-code", c.str),
          cg("`")
        )
      case Text.Plain(c) => stringFrag(c.str)
    }
  }


  private def isValidContainer(a: Node) = {
    a == root || (a.isInstanceOf[HTMLElement] && exitsClassPrefix(a.asInstanceOf[HTMLElement], "ct-c-"))
  }

  private def nodeOfContainer(a: Node) =  a.parentNode

  private[content] def readOffset(a: Node, o: Int, isEnd: Boolean): Int = {
    if (a.isInstanceOf[raw.Text] && isValidContainer(a.parentNode)) {
      rich.startPosOf(cursorOf(a)) + a.textContent.codePointCount(0, o)
    } else if (isValidContainer(a)) {
      if (o == a.childNodes.length) {
        rich.startPosOf(model.cursor.Node.moveBy(cursorOf(a.childNodes(o - 1)), 1))
      } else {
        rich.startPosOf(cursorOf(a.childNodes(o)))
      }
    } else {
      if (isEnd) {
        readOffset(a.parentNode, indexOf(a) + 1, isEnd = true)
      } else {
        readOffset(a.parentNode, indexOf(a), isEnd = false)
      }
    }
  }

  def readSelectionFromDom(): Option[IntRange] = {
    val sel = window.getSelection()
    if (sel.rangeCount == 1) {
      val range = sel.getRangeAt(0)
      if (range.collapsed) {
        val start = readOffset(range.startContainer, range.startOffset, true)
        if (start >= 0) {
          return Some(IntRange(start, start))
        }
      } else {
        val start = readOffset(range.startContainer, range.startOffset, false)
        val end = readOffset(range.endContainer, range.endOffset, true)
        if (start >= 0 && end >= 0) {
          return Some(IntRange(start, end))
        }
      }
    }
    None
  }


  def readInsertionPoint(isNode: Node = null): Int = {
    val sel = window.getSelection()
    if (sel.rangeCount == 1) {
      val range = sel.getRangeAt(0)
      readOffset(range.endContainer, range.endOffset, true)
    } else {
      -1
    }
  }

  private[content] def cursorOf(t: Node): model.cursor.Node = {
    if (debug_view) {
      window.console.log(t)
    }
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

  /**
    *
    * 1. node == text, int == pos in text, int == unicode index
    * 2. node == parent, int offset, int == -1
    */
  private[content] def posInDom(pos: Int): (Node, Int, Int) = {
    if (pos == 0) {
      if (isEmpty) {
        (root, 0, -1)
      } else if (rich.text.head.isPlain) {
        (nodeAt(Seq(0)), 0, 0)
      } else {
        (nodeChildArray(root), 0, -1)
      }
    } else if (pos == rich.size) {
      rich.text.last match {
        case plain: Text.Plain =>
          (nodeAt(Seq(rich.text.size - 1)), plain.unicode.str.length, plain.unicode.size)
        case _ =>
          (nodeChildArray(root), rich.text.size, -1)
      }
    } else {
      val ss = rich.before(pos)
      val ee = rich.after(pos)
      ss match {
        case p: Atom.PlainGrapheme =>
          (nodeAt(ss.nodeCursor), ss.text.asPlain.unicode.toStringPosition(p.unicodeUntil), p.unicodeUntil)
        case s: Atom.SpecialOrMarked =>
          ee match {
            case es: Atom.SpecialOrMarked =>
              if (ss.nodeCursor.size < ee.nodeCursor.size) { // one wraps another
                (nodeChildArray(nodeAt(ss.nodeCursor)), 0, -1)
              } else if (ss.nodeCursor == ee.nodeCursor) { // same node, empty
                (nodeChildArray(nodeAt(ss.nodeCursor)), 0, -1)
              } else { // different sibling node
                (nodeChildArray(nodeAt(model.cursor.Node.parent(ss.nodeCursor))), ss.nodeCursor.last + 1, -1)
              }
            case ep: Atom.PlainGrapheme =>
              (nodeAt(ee.nodeCursor), 0, 0)
            case ec: Atom.CodedGrapheme =>
              (nodeAt(ee.nodeCursor), 0, 0)
            case _ =>
              throw new IllegalStateException("Not possible")
          }
        case c: Atom.CodedGrapheme =>
          val unicode = ss.text.asCoded.content
          ee match {
            case es: Atom.SpecialOrMarked =>
              (nodeAt(c.nodeCursor), unicode.toStringPosition(unicode.size), unicode.size)
            case ec: Atom.CodedGrapheme =>
              (nodeAt(c.nodeCursor), unicode.toStringPosition(ec.unicodeIndex), ec.unicodeIndex)
            case _ =>
              throw new IllegalStateException("Not possible")
          }
      }
    }
  }

  private[content] def nonEmptySelectionToDomRange(range: IntRange): (Range, HTMLSpanElement) = {
    val (ss, so, _) = posInDom(range.start)
    val (es, eo, _) = posInDom(range.until)
    def createRange(a: Node, b: Int, c: Node, d: Int): Range = {
      val rr = document.createRange()
      rr.setStart(a, b)
      rr.setEnd(c, d)
      rr
    }
    (createRange(ss, so, es, eo), null)
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
