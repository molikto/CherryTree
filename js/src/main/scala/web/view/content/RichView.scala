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
import web.view.{EmptyStr, removeAllChild, theme}

import scala.scalajs.js

class RichView(documentView: DocumentView, val controller: EditorInterface,  var rich: Rich) extends ContentView[model.data.Content.Rich, model.operation.Content.Rich, model.mode.Content.Rich]  {

  private val evilChar = "\u200B"
  /**
    *
    * state
    *
    */
  private var isEmpty = rich.isEmpty

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


  private var insertEmptyTextNode: raw.Text = null
  private var insertNonEmptyTextNode: (raw.Text, Int, Int) = null
  private var astHighlight: HTMLSpanElement = null
  private var flushSubscription: Cancelable = null
  private var previousMode = if (isEmpty) -2 else -1

  private def initDom(): Unit = {
    if (dom.childNodes.length == 0) {
      dom.appendChild(rec(rich.text).render)
      if (isEmpty) initEmptyContent()
    } else {
      throw new IllegalStateException("...")
    }
  }

  initDom()

  private def clearDom(): Unit = {
    initMode(if (isEmpty) -2 else -1)
    removeAllChild(dom)
  }

  private def clearEmptyNormalMode(): Unit = {
    removeEmptyContent()
  }

  private def removeEmptyContent(): Unit = {
    removeAllChild(dom)
  }

  private def initEmptyContent(): Unit = {
    dom.appendChild(span(EmptyStr, color := theme.disalbedInfo).render)
  }

  private def initEmptyNormalMode(): Unit = {
    initEmptyContent()
    val range = document.createRange()
    range.setStart(dom, 0)
    range.setEnd(dom, 1)
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
  }

  private def cg(a: String) = span(`class` := "ct-cg", contenteditable := "false", a)

  private def rec(seq: Seq[model.data.Text]): Seq[Frag] = {
    seq.map {
      case Text.Emphasis(c) => span(
        cg("*"),
        em(`class` := "ct-em", rec(c)),
        cg("*")
      )
      case Text.Strong(c) => span(
        cg("#"),
        strong(`class` := "ct-strong", rec(c)),
        cg("#")
      )
      case Text.StrikeThrough(c) => span(
        cg("~"),
        del(`class` := "ct-del", rec(c)),
        cg("~")
      )
      case Text.Link(t, b, c) => span(
        cg("["),
        span(`class` := "ct-link", rec(t), href := b.str),
        cg("]")
      )
      case Text.Image(b, c) =>
        img(`class` := "ct-image", src := b.str)
      case Text.LaTeX(c) =>
        val a = span().render
        try {
          window.asInstanceOf[js.Dynamic].katex.render(c.str, a)
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

  private def domAt(a: Seq[Int]): Node = domAt(dom, a)

  private def domAt(parent: Node, a: Seq[Int]): Node = {
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

  private def domChildArray(parent: Node): Node = {
    if (parent.isInstanceOf[HTMLSpanElement]) {
      parent.childNodes(1)
    } else {
      parent
    }
  }

  private def domCodeText(parent: Node): Node = {
    parent.childNodes(1).childNodes(0)
  }

  private def createTempEmptyInsertTextNode(node: Node, i: Int): Unit = {
    insertEmptyTextNode = document.createTextNode(s"$evilChar$evilChar")
    val before = if (i == node.childNodes.length) null else node.childNodes(i)
    node.insertBefore(insertEmptyTextNode, before)
  }

  private def updateTempEmptyTextNodeIn(node: Node, i: Int): (Node, Int) = {
    if (insertEmptyTextNode != null) {
      if (i < node.childNodes.length && node.childNodes(i) == insertEmptyTextNode) {
        // do nothing, we are up to date
      } else {
        removeInsertEmptyTextNode()
        createTempEmptyInsertTextNode(node, i)
      }
    } else {
      createTempEmptyInsertTextNode(node, i)
    }
    (insertEmptyTextNode, 1)
  }

  private def updateExistingTextNodeIn(node: Node, i: Int): (Node, Int) = {
    removeInsertEmptyTextNode()
    val n = node.asInstanceOf[raw.Text]
    insertNonEmptyTextNode = (n, i, n.textContent.length)
    (node, i)
  }

  private def updateNonEmptyInsertCursorAt(pos: Int): (Node, Int) = {
    if (pos == 0) {
      if (rich.text.head.isPlain) {
        updateExistingTextNodeIn(domAt(Seq(0)), 0)
      } else {
        updateTempEmptyTextNodeIn(domChildArray(dom), 0)
      }
    } else if (pos == rich.size) {
      rich.text.last match {
        case plain: Text.Plain =>
          updateExistingTextNodeIn(domAt(Seq(rich.text.size - 1)), plain.unicode.str.length)
        case _ =>
          updateTempEmptyTextNodeIn(domChildArray(dom), rich.text.size)
      }
    } else {
      val ss = rich.before(pos)
      val ee = rich.after(pos)
      ss match {
        case p: Atom.PlainGrapheme =>
          updateExistingTextNodeIn(domAt(ss.nodeCursor), ss.text.asPlain.unicode.toStringPosition(p.unicodeIndex + 1))
        case s: Atom.SpecialOrMarked =>
          ee match {
            case es: Atom.SpecialOrMarked =>
              if (ss.nodeCursor.size < ee.nodeCursor.size) { // one wraps another
                updateTempEmptyTextNodeIn(domChildArray(domAt(ss.nodeCursor)), 0)
              } else if (ss.nodeCursor == ee.nodeCursor) { // same node, empty
                if (ee.text.isCode) {
                  updateExistingTextNodeIn(domCodeText(domAt(ss.nodeCursor)), 0)
                } else {
                  updateTempEmptyTextNodeIn(domChildArray(domAt(ss.nodeCursor)), 0)
                }
              } else { // different sibling node
                updateTempEmptyTextNodeIn(domChildArray(domAt(ss.nodeCursor.dropRight(1))), ss.nodeCursor.last + 1)
              }
            case ep: Atom.PlainGrapheme =>
              updateExistingTextNodeIn(domAt(ee.nodeCursor), 0)
            case ec: Atom.CodedGrapheme =>
              updateExistingTextNodeIn(domCodeText(domAt(ee.nodeCursor)), 0)
            case _ =>
              throw new IllegalStateException("Not possible")
          }
        case c: Atom.CodedGrapheme =>
          val unicode = ss.text.asCoded.content
          ee match {
            case es: Atom.SpecialOrMarked =>
              updateExistingTextNodeIn(domCodeText(domAt(ee.nodeCursor)), unicode.toStringPosition(unicode.size))
            case ec: Atom.CodedGrapheme =>
              updateExistingTextNodeIn(domCodeText(domAt(ee.nodeCursor)), unicode.toStringPosition(ec.unicodeIndex))
            case _ =>
              throw new IllegalStateException("Not possible")
          }
      }
    }
  }

  private def nonEmptySelectionToDomRange(range: IntRange): (Range, HTMLSpanElement) = {
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
      ss.text.isCode) {
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
      val start = if (ss.isInstanceOf[Atom.PlainGrapheme]) {
        val text = domAt(ss.nodeCursor)
        val s = ss.text.asPlain.unicode.toStringPosition(ss.asInstanceOf[Atom.PlainGrapheme].unicodeIndex)
        (text, s)
      } else {
        assert(ss.delimitationStart || ss.isInstanceOf[Atom.Marked])
        val node = domChildArray(domAt(ss.nodeCursor.dropRight(1)))
        (node, ss.nodeCursor.last)
      }
      val end = if (ee.isInstanceOf[Atom.PlainGrapheme]) {
        val text = domAt(ee.nodeCursor)
        val e = ee.text.asPlain.unicode.toStringPosition(ss.asInstanceOf[Atom.PlainGrapheme].unicodeIndex + ss.size)
        (text, e)
      } else {
        assert(ss.delimitationEnd || ss.isInstanceOf[Atom.Marked])
        val node = domChildArray(domAt(ee.nodeCursor.dropRight(1)))
        (node, ee.nodeCursor.last + 1)
      }
      (createRange(start._1, start._2, end._1, end._2), null)
    }
  }


  private def clearFormattedNodeHighlight(): Unit = {
    if (astHighlight != null) {
      astHighlight.style.backgroundColor = "#FFFFFF00"
      astHighlight = null
    }
  }

  private def addFormattedNodeHighlight(_5: HTMLSpanElement): Unit = {
    astHighlight = _5
    _5.style.backgroundColor = theme.astHighlight
  }

  /**
    *
    *
    * events
    *
    */


  event("compositionstart", (a: CompositionEvent) => {
    if (isInserting) controller.disableStateUpdate = true
    else preventDefault(a)
  })

  event("compositionupdate", (a: CompositionEvent) => {
    if (!isInserting) preventDefault(a)
  })

  event("compositionend", (a: CompositionEvent) => {
    if (isInserting) controller.disableStateUpdate = false
    else preventDefault(a)
  })

  event("input", (a: Event) => {
    // TODO only accept single node text changes, or subparagraph changes??
    // formatBold
    // Contenteditable	"insertText", "insertCompositionText", "insertFromComposition", "formatSetBlockTextDirection", "formatSetInlineTextDirection", "formatBackColor", "formatFontColor", "formatFontName", "insertLink"	Yes	null	Non-empty Array
    //Contenteditable	"insertFromPaste", "insertFromDrop", "insertReplacementText", "insertFromYank"	null
    if (isInserting) {
      val inputType = a.asInstanceOf[js.Dynamic].inputType.asInstanceOf[String]
      if (inputType == "insertText" || inputType == "insertCompositionText") {
        // should be pick up by our keyboard handling
        controller.flush()
      } else {
        window.console.log(a)
        preventDefault(a)
      }
    } else {
      window.console.log(a)
      preventDefault(a)
    }
  })


  private def mergeTextsFix(center: raw.Text): String = {
    if (center.wholeText != center.textContent) {
      center.textContent = center.wholeText
      var previous = center.previousSibling
      var next = center.nextSibling
      while (previous != null && previous.isInstanceOf[raw.Text]) {
        previous.parentNode.removeChild(previous)
        previous = center.previousSibling
      }
      while (next != null && next.isInstanceOf[raw.Text]) {
        next.parentNode.removeChild(next)
        next = center.nextSibling
      }
    }
    center.textContent
  }

  /**
    *
    * mode rendering
    *
    * in mode rendering we always assume the content is rendered correctly
    *
    *
    */

  private def flushInsertionMode(): Unit = {
    if (insertEmptyTextNode != null) {
      val tc = insertEmptyTextNode.textContent
      assert(tc.startsWith(evilChar))
      assert(tc.endsWith(evilChar))
      val str = tc.substring(1, tc.length - 1)
      if (str.length > 0) {
        insertNonEmptyTextNode = (insertEmptyTextNode, str.length, str.length)
        insertNonEmptyTextNode._1.textContent = str
        insertEmptyTextNode = null
        controller.onInsertRichTextAndViewUpdated(Unicode(str))
      }
    } else if (insertNonEmptyTextNode != null) {
      val newContent = mergeTextsFix(insertNonEmptyTextNode._1)
      val insertion = newContent.substring(insertNonEmptyTextNode._2, insertNonEmptyTextNode._2 + newContent.length - insertNonEmptyTextNode._3)
      if (insertion.length > 0) {
        insertNonEmptyTextNode = (insertNonEmptyTextNode._1, insertNonEmptyTextNode._2 + insertion.length, newContent.length)
        controller.onInsertRichTextAndViewUpdated(Unicode(insertion))
      }
    }
  }


  private def removeInsertEmptyTextNode(): Unit = {
    if (insertEmptyTextNode != null) {
      if (insertEmptyTextNode.parentNode != null)
        insertEmptyTextNode.parentNode.removeChild(insertEmptyTextNode)
      insertEmptyTextNode = null
    }
  }

  private def clearInsertionMode(): Unit = {
    removeInsertEmptyTextNode()
    insertNonEmptyTextNode = null
    if (flushSubscription != null) {
      flushSubscription.cancel()
      flushSubscription = null
    }
  }

  private def clearVisualMode(): Unit = {
    clearSelection()
  }

  private def updateVisualMode(fix: IntRange, move: IntRange, fromUser: Boolean): Unit = {
    val (r1,_) = nonEmptySelectionToDomRange(fix)
    val (r2,_) = nonEmptySelectionToDomRange(move)
    val range = document.createRange()
    if (r1.compareBoundaryPoints(Range.START_TO_START, r2) == -1) {
      range.setStart(r1.startContainer, r1.startOffset)
    } else {
      range.setStart(r2.startContainer, r2.startOffset)
    }
    if (r1.compareBoundaryPoints(Range.END_TO_END, r2) == 1) {
      range.setEnd(r1.endContainer, r1.endOffset)
    } else {
      range.setEnd(r2.endContainer, r2.endOffset)
    }
    setSelection(range, fromUser)
  }

  private def clearSelection(): Unit = {
    val sel = window.getSelection
    if (sel.rangeCount > 0) sel.removeAllRanges
  }

  private def clearNormalMode(): Unit = {
    clearSelection()
    clearFormattedNodeHighlight()
  }


  private def updateInsertMode(pos: Int, fromUser: Boolean): Unit = {
    if (flushSubscription == null) {
      flushSubscription = observe(controller.flushes.doOnNext(_ => {
        flushInsertionMode()
      }))
    }
    val range = document.createRange()
    val start = if (isEmpty) {
      updateTempEmptyTextNodeIn(dom, 0)
    }  else {
      updateNonEmptyInsertCursorAt(pos)
    }
    range.setStart(start._1, start._2)
    range.setEnd(start._1, start._2)
    setSelection(range, fromUser)
    mergeTextsFix(start._1.asInstanceOf[raw.Text])
  }

  override def selectionRect: Rect = {
    val range = window.getSelection().getRangeAt(0)
    web.view.toRect(range.getBoundingClientRect())
  }


  private def updateNormalMode(r: IntRange, fromUser: Boolean): Unit = {
    val (range, light) = nonEmptySelectionToDomRange(r)
    setSelection(range, fromUser = fromUser)
    if (light != astHighlight) clearFormattedNodeHighlight()
    if (light != null) addFormattedNodeHighlight(light)
  }

  private def setSelection(range: Range, fromUser: Boolean): Unit = {
//    if (fromUser) {
//      val r1 = dom.getBoundingClientRect()
//      val r2 = documentView.dom.getBoundingClientRect()
//      val rect = if (r1.height < r2.height) {
//        r1
//      } else {
//        range.getBoundingClientRect()
//      }
//      web.view.scrollInToViewIfNotVisible(rect, documentView.dom)
//    }
//    val top = documentView.dom.scrollTop
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
//    documentView.dom.scrollTop = top
  }

  override def clearMode(): Unit = {
    initMode(if (isEmpty) -2 else -1)
    documentView.unmarkEditable(dom)
  }

  override def initMode(): Unit = {
    documentView.markEditable(dom)
  }

  private def isInserting = flushSubscription != null

  private def initMode(i: Int): Unit = {
    if (previousMode != i) {
      if (debug_view) println(s"mode change from  $previousMode to $i")
      if (previousMode == 0) {
        clearInsertionMode()
      } else if (previousMode == 1) {
        clearVisualMode()
      } else if (previousMode == 2) {
        clearNormalMode()
      } else if (previousMode == 3) {
        clearEmptyNormalMode()
      } else if (previousMode == -2) {
        removeEmptyContent()
      }
      if (i == 3) {
        initEmptyNormalMode()
      } else if (i == -2) {
        initEmptyContent()
      }
      previousMode = i
    }
  }

  override def updateMode(aa: mode.Content.Rich, viewUpdated: Boolean, fromUser: Boolean): Unit = {
    aa match {
      case mode.Content.RichInsert(pos) =>
        initMode(0)
        updateInsertMode(pos, fromUser)
      case mode.Content.RichVisual(fix, move) =>
        initMode(1)
        updateVisualMode(fix, move, fromUser)
      case mode.Content.RichNormal(range) =>
        if (isEmpty) {
          initMode(3)
        } else {
          initMode(2)
          updateNormalMode(range, fromUser)
        }
    }
  }

  override def updateContent(data: model.data.Content.Rich, c: operation.Content.Rich, viewUpdated: Boolean): Unit = {
    rich = data.content
    isEmpty = rich.isEmpty
    if (!viewUpdated) {
     // val cs = c.asInstanceOf[operation.Content.Rich]
      // TODO incrementally update dom remember to clear the empty range when needed
      clearDom()
      initDom()
    }
  }

  /**
    * will also remove from parent
    */
  override def destroy(): Unit = {
    clearMode()
    super.destroy()
  }

}
