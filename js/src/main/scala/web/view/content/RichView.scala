package web.view.content

import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Event, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import view.EditorInterface
import web.view.doc.DocumentView
import web.view.{EmptyStr, removeAllChild, theme}

import scala.scalajs.js

class RichView(documentView: DocumentView, val controller: EditorInterface,  var rich: Rich) extends ContentView[model.data.Content.Rich, model.operation.Content.Rich, model.mode.Content.Rich]  {

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

  dom = p(`class` := "ct-content").render


  dom.style = "outline: 0px solid transparent;"


  private var insertEmptyTextNode: raw.Text = null
  private var insertNonEmptyTextNode: raw.Text = null
  private var insertNonEmptyTextNodeStartIndex: Int = 0
  private var insertNonEmptyTextLength: Int = 0
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

  private def rec(seq: Seq[model.data.Text]): Seq[Frag] = {
    seq.map {
      case Text.Emphasis(c) => span(
        span(`class` := "ct-cg",contenteditable := "false", "*"),
        em(`class` := "ct-em", rec(c)),
        span(`class` := "ct-cg",contenteditable := "false", "*")
      )
      case Text.Strong(c) => span(
        span(`class` := "ct-cg",contenteditable := "false", "#"),
        strong(`class` := "ct-strong", rec(c)),
        span(`class` := "ct-cg",contenteditable := "false", "#") // LATER ** char as a single char
      )
      case Text.StrikeThrough(c) => span(
        span(`class` := "ct-cg",contenteditable := "false", "-"),
        del(`class` := "ct-del", rec(c)),
        span(`class` := "ct-cg",contenteditable := "false", "-")
      )
      case Text.Link(t, b, c) => span(
        span(`class` := "ct-cg",contenteditable := "false", "["),
        span(`class` := "ct-link", rec(t), href := b.toString),
        span(`class` := "ct-cg",contenteditable := "false", "]")
      )
      case Text.Image(b, c) =>
        img(verticalAlign := "bottom", src := b.toString)
      case Text.LaTeX(c) =>
        val a = span().render
        window.asInstanceOf[js.Dynamic].katex.render(c.toString, a)
        span(contenteditable := "false", `class` := "ct-latex",
          span("\u200B"), // don't fuck with my cursor!!!
          a,
          span("\u200b")
        )
      case Text.Code(c) =>
        span(
          span(`class` := "ct-cg", contenteditable := "false", "`"),
          code(`class` := "ct-code", c.toString),
          span(`class` := "ct-cg", contenteditable := "false", "`")
        )
      case Text.Plain(c) => stringFrag(c.toString)
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
        var i = 0
        while (i <= a.head) {
          c = childArray.childNodes(i)
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
    insertEmptyTextNode = document.createTextNode("")
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
    (insertEmptyTextNode, 0)
  }

  private def updateExistingTextNodeIn(node: Node, i: Int): (Node, Int) = {
    removeInsertEmptyTextNode()
    insertNonEmptyTextNode = node.asInstanceOf[raw.Text]
    insertNonEmptyTextNodeStartIndex = i
    insertNonEmptyTextLength = insertNonEmptyTextNode.textContent.length
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
          updateExistingTextNodeIn(domAt(Seq(rich.text.size - 1)), plain.size)
        case _ =>
          updateTempEmptyTextNodeIn(domChildArray(dom), rich.text.size)
      }
    } else {
      val ss = rich.infoSkipLeftAttributes(pos - 1)
      val ee = rich.infoSkipRightAttributes(pos)
      if (ss.ty == InfoType.Plain) {
        if (ee.ty == InfoType.Special || ee.ty == InfoType.Plain) {
          updateExistingTextNodeIn(domAt(ss.nodeCursor), ss.text.asPlain.unicode.toStringPosition(ss.positionInUnicode + 1))
        } else {
          throw new IllegalStateException("Not possible")
        }
      } else if (ss.ty == InfoType.Special) {
        if (ee.ty == InfoType.Special) {
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
        } else if (ee.ty == InfoType.Plain) {
          updateExistingTextNodeIn(domAt(ee.nodeCursor), 0)
        } else if (ee.ty == InfoType.Coded) {
          updateExistingTextNodeIn(domCodeText(domAt(ee.nodeCursor)), 0)
        } else {
          throw new IllegalStateException("Not possible")
        }
      } else if (ss.ty == InfoType.Coded) {
        val unicode = ss.text.asCoded.content
        if (ee.ty == InfoType.Special) {
          updateExistingTextNodeIn(domCodeText(domAt(ee.nodeCursor)), unicode.toStringPosition(unicode.size))
        } else if (ee.ty == InfoType.Coded) {
          updateExistingTextNodeIn(domCodeText(domAt(ee.nodeCursor)), unicode.toStringPosition(ee.positionInUnicode))
        } else {
          throw new IllegalStateException("Not possible")
        }
      } else {
        throw new IllegalStateException("Not possible")
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
    // there are three cases of a selection
    // a subparagraph, a sub-code, a delimiter of format/code node
    val ss = rich.info(range.start)
    val ee = rich.info(range.until - 1)
    if (ss.ty == InfoType.Coded &&
      ee.ty == InfoType.Coded &&
      ss.nodeCursor == ee.nodeCursor &&
      ss.text.isCode) {
      val codeText = domCodeText(domAt(ss.nodeCursor))
      val ast = ss.text.asCoded
      val sss = ast.content.toStringPosition(ss.positionInUnicode)
      val eee = ast.content.toStringPosition(ee.positionInUnicode + 1)
      (createRange(codeText, sss, codeText, eee), null)
    } else if (range.size == 1 &&
      ss.ty  == InfoType.Special &&
      SpecialChar.startsEnds.contains(ss.specialChar) &&
      !ss.text.isAtomicViewed) {
      val isStart = SpecialChar.starts.contains(ss.specialChar)
      val a = domAt(ss.nodeCursor).asInstanceOf[HTMLSpanElement]
      val range = if (isStart) (0, 1) else (2, 3)
      (createRange(a, range._1, a, range._2), a)
    } else {
      val start = if (ss.ty == InfoType.Plain) {
        val text = domAt(ss.nodeCursor)
        val s = ss.text.asPlain.unicode.toStringPosition(ss.positionInUnicode)
        (text, s)
      } else {
        assert(ss.isStart)
        val node = domChildArray(domAt(ss.nodeCursor.dropRight(1)))
        (node, ss.nodeCursor.last)
      }
      val end = if (ee.ty == InfoType.Plain) {
        val text = domAt(ee.nodeCursor)
        val e = ee.text.asPlain.unicode.toStringPosition(ss.positionInUnicode + 1)
        (text, e)
      } else {
        assert(ee.isEnd)
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
    else cancel(a)
  })

  event("compositionupdate", (a: CompositionEvent) => {
    if (!isInserting) cancel(a)
  })

  event("compositionend", (a: CompositionEvent) => {
    if (isInserting) controller.disableStateUpdate = false
    else cancel(a)
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
      }
    } else {
      window.console.log(a)
      cancel(a)
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
      val str = mergeTextsFix(insertEmptyTextNode)
      // this is really ugly, but somehow Chrome create a new TextNode??
      if (str.length > 0) {
        insertEmptyTextNode.textContent = str
        insertNonEmptyTextNode = insertEmptyTextNode
        insertNonEmptyTextLength = str.length
        insertNonEmptyTextNodeStartIndex = insertNonEmptyTextLength
        insertEmptyTextNode = null
        controller.onInsertRichTextAndViewUpdated(Unicode(str))
      }
    } else if (insertNonEmptyTextNode != null) {
      val newContent = mergeTextsFix(insertNonEmptyTextNode)
      val insertion = newContent.substring(
        insertNonEmptyTextNodeStartIndex, insertNonEmptyTextNodeStartIndex + newContent.length - insertNonEmptyTextLength)
      if (insertion.length > 0) {
        insertNonEmptyTextLength = newContent.length
        insertNonEmptyTextNodeStartIndex += insertion.length
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

  private def updateVisualMode(fix: IntRange, move: IntRange): Unit = {
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
    setSelection(range)
  }

  private def clearSelection(): Unit = {
    val sel = window.getSelection
    if (sel.rangeCount > 0) sel.removeAllRanges
  }

  private def clearNormalMode(): Unit = {
    clearSelection()
    clearFormattedNodeHighlight()
  }

  private def updateInsertMode(pos: Int): Unit = {
    if (flushSubscription == null) {
      flushSubscription = observe(controller.flushes.doOnNext(_ => {
        flushInsertionMode()
      }))
    }
    val range = document.createRange()
    if (isEmpty) {
      if (insertEmptyTextNode != null) {
        assert(dom.childNodes.length == 1)
      } else {
        insertEmptyTextNode = document.createTextNode("")
        dom.appendChild(insertEmptyTextNode)
        range.setStart(insertEmptyTextNode, 0)
        range.setEnd(insertEmptyTextNode, 0)
      }
    }  else {
      val start = updateNonEmptyInsertCursorAt(pos)
      range.setStart(start._1, start._2)
      range.setEnd(start._1, start._2)
    }
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
  }


  private def updateNormalMode(r: IntRange): Unit = {
    val (range, light) = nonEmptySelectionToDomRange(r)
    setSelection(range)
    if (light != astHighlight) clearFormattedNodeHighlight()
    if (light != null) addFormattedNodeHighlight(light)
  }

  private def setSelection(range: Range): Unit = {
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
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
      if (debugView) println(s"mode change from  $previousMode to $i")
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

  override def updateMode(aa: mode.Content.Rich, viewUpdated: Boolean): Unit = {
    aa match {
      case mode.Content.RichInsert(pos) =>
        initMode(0)
        updateInsertMode(pos)
      case mode.Content.RichVisual(fix, move) =>
        initMode(1)
        updateVisualMode(fix, move)
      case mode.Content.RichNormal(range) =>
        if (isEmpty) {
          initMode(3)
        } else {
          initMode(2)
          updateNormalMode(range)
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
    documentView.unmarkEditableIfEditable(dom)
    super.destroy()
  }
}
