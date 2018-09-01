package web.ui.content

import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw.{CompositionEvent, Element, ErrorEvent, Event, EventTarget, HTMLElement, HTMLSpanElement, MouseEvent, Node, NodeList, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.ui
import web.ui.doc.{DocumentView, LaTeXMacroCache}
import web.view._
import web.ui.content.ContentViewEditor.General
import web.ui._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js



/**
  *
  *
  *
  * THE EMPTY INSERT POINT IS EVIL!!!!!! CHECK YOUR CODE IS PREPARED FOR IT!!!
  *
  *
  */
object RichView {

  val onImageError: js.Function1[ErrorEvent, _] = (e: ErrorEvent) => {
    val el = e.target.asInstanceOf[HTMLElement]
    window.console.log("image loading error")
    el.style.width = "12px"
    el.style.height= "12px"
  }

}
class RichView(initData: model.data.Content.Rich, val isHr: Boolean) extends ContentView.Rich {


  private def emptyStr = if (isHr) "⁂  ⁂  ⁂" else ui.EmptyStr

  def rich: Rich = contentData.content

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

  dom = p(`class` := (if (isHr) "ct-rich-hr ct-rich" else "ct-rich")).render

  updateContent(initData)

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


  private[content] def clearDom(): Unit = {
    setPreviousModeToEmpty()
    removeAllChild(root)
  }


  private[content] def removeEmptyContent(): Unit = {
    removeAllChild(root)
  }

  private[content] def initEmptyContent(): Unit = {
    if (root.childNodes.length == 1 && root.textContent == emptyStr && root.childNodes(0).isInstanceOf[Span]
      && root.childNodes(0).asInstanceOf[Span].className == "ct-hint-color") {
    } else {
      removeAllChild(root)
      root.appendChild(span(`class` := "ct-hint-color", emptyStr).render)
    }
  }

  private def cg(a: String, extraClass: String = "") = span(`class` := "ct-cg " + extraClass,
    contenteditable := "false", a)

  override def tempEditableTempDuringSelectionChange(b: Boolean): Unit = {
    jQ(dom).find(".ct-cg, .ct-cg-atom").attr("contenteditable", b.toString)
  }


  override def refreshLaTeX(): Unit = {
    val ct = dom.getElementsByClassName("ct-latex-p")
    var i = 0
    while (i < ct.length) {
      val el = ct.item(i).asInstanceOf[HTMLElement]
      val str = el.getAttribute("data")
      el.removeChild(el.childNodes(1))
      LaTeXMacroCache.renderLaTeX(el, str, 1)
      i += 1
    }
  }


  /**
    * ct-cg is control glyph
    * ct-c-xxx and root is valid container
    * text node and ct-cg-node is valid container childs
    */
  private def rec(seq: Seq[model.data.Text]): Seq[Frag] = {
    seq.map {
      case Text.Emphasis(c) => span(
        `class` := "ct-cg-node",
        cg("*"),
        em(`class` := "ct-c-em", rec(c)),
        cg("*")
      )
      case Text.Strong(c) => span(
        `class` := "ct-cg-node",
        cg("*", "ct-cg-shadow"),
        strong(`class` := "ct-c-strong", rec(c)),
        cg("*", "ct-cg-shadow")
      )
      case Text.StrikeThrough(c) => span(
        `class` := "ct-cg-node",
        cg("~"),
        del(`class` := "ct-c-del", rec(c)),
        cg("~")
      )
      case l@Text.Link(t, b, c) =>
        val tt: String = if (c.isEmpty) b.str else s"${c.str}\n${b.str}"
        span(
          `class` := "ct-cg-node",
          title := tt,
          cg("["),
          span(`class` := (if (l.isNodeRef) "ct-c-link-node" else "ct-c-link"), rec(t)),
          cg("]")
        )
      case Text.Image(b, c) =>
        val sp = span(
          `class` := "ct-cg-node ct-cg-atom",
          contenteditable := false,
          span(EvilChar) // don't fuck with my cursor!!!
        ).render
        if (b.isEmpty) {
          sp.appendChild(warningInline("empty image").render)
        } else {
          sp.appendChild(img(`class` := "ct-image", title := c.str, src := b.str, onerror := {e: Event => {
            sp.removeChild(sp.childNodes(1))
            sp.insertBefore(errorInline("image error").render, sp.childNodes(1))
          }}).render)
        }
        sp.appendChild(span(EvilChar).render)
        sp: Frag
      case Text.HTML(c) =>
        val a = span(
         contenteditable := "false",
          `class` := "ct-cg-node ct-cg-atom",
          span(EvilChar) // don't fuck with my cursor!!!
        ).render
        if (c.isBlank) {
          a.appendChild(warningInline("empty inline HTML").render)
        } else {
          try {
            val b = span(
              `class` := "ct-inline-html"
            ).render
            b.innerHTML = c.str
            a.appendChild(b)
          } catch {
            case err: Throwable =>
              a.appendChild(errorInline("inline HTML error", err).render)
          }
        }
        a.appendChild(span(EvilChar).render)
        a: Frag
      case Text.LaTeX(c) =>
        val a = span(
          contenteditable := "false",
          `class` := "ct-cg-node ct-cg-atom ct-latex-p",
          data := c.str,
          span(EvilChar)
        ).render
        LaTeXMacroCache.renderLaTeX(a, c.str, 1)
        a.appendChild(span(EvilChar).render)
        a: Frag
      case Text.Code(c) =>
        span(
          `class` := "ct-cg-node",
          cg("`"),
          span(`class` := "ct-c-code", c.str),
          cg("`")
        )
      case Text.Plain(c) => stringFrag(c.str)
    }
  }


  def atomicParentOf(target: Node): (HTMLElement, Int, Int) = {
    var t = target
    while (t != null && t != dom) {
      t match {
        case element: HTMLElement =>
          if (element.classList.contains("ct-cg-atom")) {
            val cur = cursorOf(element)
            return (element, contentData.content.startPosOf(cur), contentData.content.startPosOf(model.cursor.Node.moveBy(cur, 1)))
          }
        case _ =>
      }
      t = t.parentNode
    }
    null
  }

  private def isValidContainer(a: Node) = {
    a == root || (a.isInstanceOf[HTMLElement] && exitsClassPrefix(a.asInstanceOf[HTMLElement], "ct-c-"))
  }

  private def nodeOfContainer(a: Node) =  a.parentNode


  private def normalizeOffset(parentNode: Node, o: Int): Int = {
    if (extraNode != null && extraNode.parentNode == parentNode) {
      if (indexOf(extraNode) <= o) {
        o - 1
      } else {
        o
      }
    } else {
      o
    }
  }

  private[content] def readOffset(a: Node, o: Int, isEnd: Boolean): Int = {
    val head = document.getElementsByTagName("head").item(0)
    if (dom.contains(a)) {
      if (a == extraNode) {
        readOffsetNormalizedIndex(a.parentNode, indexOf(a), isEnd)
      } else {
        readOffsetNormalizedIndex(a, normalizeOffset(a, o), isEnd)
      }
    } else if (a.contains(dom)) {
      var pp: Node = dom
      while (pp.parentNode != a) {
        pp = pp.parentNode
      }
      if (model.debug_selection) window.console.log("read offset contained", pp, indexOf(pp), a, o)
      if (indexOf(pp) <= o) 0 else rich.size
    } else {
      if ((dom.compareDocumentPosition(a) & Node.DOCUMENT_POSITION_FOLLOWING) != 0) {
        if (model.debug_selection) window.console.log("read offset node before")
        0
      } else {
        if (model.debug_selection) window.console.log("read offset node after")
        rich.size
      }
    }
  }

  /**
    */
  private def readOffsetNormalizedIndex(a: Node, o: Int, isEnd: Boolean): Int = {
    val ret = if (a.isInstanceOf[raw.Text] && isValidContainer(a.parentNode)) { // a text node inside a valid container
      rich.startPosOf(cursorOf(a)) + a.textContent.codePointCount(0, o)
    } else if (isValidContainer(a)) { // a node inside a valid container
      if (o >= childNodesLength(a)) {
        val cur = cursorOf(childNodes(a, o - 1))
        rich.startPosOf(cur) + rich(cur).size
      } else {
        rich.startPosOf(cursorOf(childNodes(a, o)))
      }
    } else {
      a match {
        case el: raw.Text if el.parentNode.asInstanceOf[HTMLElement].classList.contains("ct-cg") =>
          val cg = el.parentNode.asInstanceOf[HTMLElement]
          val sty = cg.parentNode.asInstanceOf[HTMLElement]
          if (indexOf(cg) == 0) {
            if (o == 0) {
              rich.startPosOf(cursorOf(sty))
            } else {
              rich.startPosOf(cursorOf(sty)) + 1
            }
          } else {
            val cur = cursorOf(sty)
            if (o == 0) {
              rich.startPosOf(cur) + rich(cur).asDelimited.contentSize + 1
            } else {
              rich.startPosOf(cur) + rich(cur).size
            }
          }
        case _ =>
          if (isEnd) {
            readOffsetNormalizedIndex(a.parentNode, indexOf(a, extraNode) + 1, isEnd = true)
          } else {
            readOffsetNormalizedIndex(a.parentNode, indexOf(a, extraNode), isEnd = false)
          }
      }
    }
    if (model.debug_selection) {
      window.console.log("read offset", a, o, isEnd, ret)
    }
    ret
  }

  def readSelectionFromDom(): Option[(IntRange, Boolean)] = {
    val sel = window.getSelection()
    if (sel.rangeCount >= 1) {
      if (model.debug_selection) {
        window.console.log("read selection", sel)
      }
      if (isEmpty) {
        return Some((IntRange(0, 0), true))
      } else {
        val range = sel.getRangeAt(0)
        if (range.collapsed) {
          val start = readOffset(range.startContainer, range.startOffset, false)
          val end = readOffset(range.endContainer, range.endOffset, true)
          if (start != end) {
            val a = rich.after(start).nodeCursor
            val node = elementParent(nodeAt(a))
            val r1 = node.getBoundingClientRect()
            val left = r1.left
            val right = r1.right
            var rect = range.getBoundingClientRect()
            val c = if (rect.width == 0 && rect.left == 0 && rect.top == 0 && rect.height == 0) {
              if (range.startOffset < range.startContainer.childNodes.length) {
                rect = elementParent(range.startContainer.childNodes(range.startOffset)).getBoundingClientRect()
                (rect.left + rect.right) / 2
              } else {
                rect = elementParent(range.startContainer).getBoundingClientRect()
                rect.right
              }
            } else {
              (rect.left + rect.right) / 2
            }
            if (model.debug_selection) {
              window.console.log("finding insertion point for atomic", node, range, left, right, c)
            }
            if (Math.abs(left - c) <= Math.abs(right - c)) {
              return Some((IntRange(start, start), true))
            } else {
              return Some((IntRange(end, end), true))
            }
          } else {
            return Some((IntRange(start, start), true))
          }
        } else {
          def isStart(a: Node, b: Int) = a == range.startContainer && b == range.startOffset
          val moveIsEnd = !isStart(sel.focusNode, sel.focusOffset)
          val fix = readOffset(sel.anchorNode, sel.anchorOffset, !moveIsEnd)
          val move = readOffset(sel.focusNode, sel.focusOffset, moveIsEnd)
          if (moveIsEnd) {
            return Some((IntRange(fix, Math.max(fix, move)), true))
          } else {
            return Some((IntRange(move, Math.max(move, fix)), false))
          }
        }
      }
    }
    None
  }


  private[content] def readPlainInsertionPointBeforeFlush(isNode: Node = null): Int = {
    val sel = window.getSelection()
    if (sel.rangeCount >= 1) {
      if (isEmpty) {
        0
      } else {
        if (model.debug_selection) {
          window.console.log("read insertion", sel)
        }
        val range = sel.getRangeAt(0)
        return readOffset(range.endContainer, range.endOffset, true)
      }
    }
    -1
  }

  private[content] def cursorOf(t: Node): model.cursor.Node = {
    if (model.debug_view) {
      if (t.isInstanceOf[raw.Text] || t.asInstanceOf[HTMLElement].classList.contains("ct-cg-node")) {
      } else {
        window.console.log("cursor of given wrong kind of data", t)
        assert(false)
      }
      if (!isValidContainer(t.parentNode)) {
        window.console.log("cursor of have wrong parent", t.parentNode)
        assert(false)
      }
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


  private def childNodes(childArray: Node, j: Int): Node = {
    var c: Node = null
    if (extraNode == null || extraNode.parentNode != childArray) {
      c = childArray.childNodes(j)
    } else {
      c = childArray.firstChild
      if (c == extraNode) c = c.nextSibling
      var i = 0
      while (i < j) {
        c = c.nextSibling
        if (c != extraNode) {
          i += 1
        }
      }
    }
    c
  }

  private def childNodesLength(a: Node) = {
    if (extraNode == null || extraNode.parentNode != a) {
      a.childNodes.length
    } else {
      a.childNodes.length - 1
    }
  }

  private[content] def nodeAt(a: Seq[Int]): Node = nodeAt(root, a)

  private[content] var extraNode: raw.Text = null

  private[content] def nodeAt(parent: Node, a: Seq[Int]): Node = {
    if (a.isEmpty) {
      parent
    } else {
      val childArray = nodeChildArray(parent)
      val c = childNodes(childArray, a.head)
      nodeAt(c, a.tail)
    }
  }

  private[content] def nodeChildArray(parent: Node): Node = {
    if (parent.isInstanceOf[HTMLSpanElement]) {
      childNodes(parent, 1)
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
    val ret = if (pos == 0) {
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
    if (model.debug_selection) {
      window.console.log(s"pos $pos in dom", ret._1, ret._2, ret._3)
    }
    ret
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


  def refreshDom(): Unit = {
    clearDom()
    initDom()
  }


  protected override def onUpdateContent(dat: model.data.Content.Rich): Unit = {
    refreshDom()
  }
}
