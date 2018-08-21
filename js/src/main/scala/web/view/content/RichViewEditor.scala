package web.view.content

import model._
import model.data._
import model.mode.Content
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Paragraph
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLDivElement, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view._

import scala.scalajs.js

class RichViewEditor(val documentView: DocumentView, val controller: EditorInterface, override val contentView: RichView) extends ContentViewEditor[model.data.Content.Rich, model.operation.Content.Rich, model.mode.Content.Rich](contentView)  {

  /**
    * selection
    */

  private var rangeSelection: Range = null

  override def selectionRect: Rect = {
    web.view.toRect(if (rangeSelection != null) {
      rangeSelection.getBoundingClientRect()
    } else if (documentView.hasInsertion) {
      documentView.insertion.getBoundingClientRect()
    } else {
      contentView.dom.getBoundingClientRect()
    })
  }

  private def clearRangeSelection(): Unit = {
    if (rangeSelection != null) {
      removeAllChild(documentView.selections)
    }
    rangeSelection = null
  }

  private def setRangeSelection(range: Range, fromUser: Boolean): Unit = {
    clearRangeSelection()
    val dom = documentView.selections
    rangeSelection = range
    val p = dom.getBoundingClientRect()
    val rects = range.getClientRects()
    for (i <- 0 until rects.length) {
      val rect = rects(i)
      dom.appendChild(
        div(
          `class` := "ct-rich-selection",
          contenteditable := "false",
          left := rect.left - p.left,
          top := rect.top - p.top,
          width := rect.width,
          height := rect.height
        ).render)
    }

  }

  import contentView._

  private var insertEmptyTextNode: (raw.Text, Int) = null
  private var insertNonEmptyTextNode: (raw.Text, String, Int) = null
  private var astHighlight: HTMLSpanElement = null
  private var flushSubscription: Cancelable = null
  private var previousMode = if (isEmpty) -2 else -1



  def removeInsertEmptyTextNode(): Unit = {
    if (extraNode != null) {
      removeFromChild(extraNode)
      extraNode = null
      assert(insertEmptyTextNode != null)
      insertEmptyTextNode = null
    } else {
      assert(insertEmptyTextNode == null)
    }
  }

  defer(_ => {
    clearMode()
    clearEditor()
  })

  private def updateInsertCursorAt(pos: Int): (Node, Int) = {

    def createTempEmptyInsertTextNode(node: Node, i: Int, pos: Int): Unit = {
      val extra = document.createTextNode(s"${RichView.EvilChar}${RichView.EvilChar}")
      extraNode = extra
      insertEmptyTextNode = (extra, pos)
      val before = if (i == node.childNodes.length) null else node.childNodes(i)
      node.insertBefore(extra, before)
    }


    def updateTempEmptyTextNodeIn(node: Node, i: Int): (Node, Int) = {
      if (insertEmptyTextNode != null) {
        if (i < node.childNodes.length && node.childNodes(i) == insertEmptyTextNode._1) {
          // do nothing, we are up to date
        } else {
          removeInsertEmptyTextNode()
          createTempEmptyInsertTextNode(node, i, pos)
        }
      } else {
        createTempEmptyInsertTextNode(node, i, pos)
      }
      (insertEmptyTextNode._1, 1)
    }

    def updateExistingInsertingTextNodeIn(node: Node, i: Int, atUnicode: Int): (Node, Int) = {
      removeInsertEmptyTextNode()
      assert(node != null)
      val n = node.asInstanceOf[raw.Text]
      insertNonEmptyTextNode = (n, n.textContent, pos - atUnicode)
      (node, i)
    }


    if (pos == 0) {
      if (isEmpty) {
        updateTempEmptyTextNodeIn(root, 0)
      } else if (rich.text.head.isPlain) {
        updateExistingInsertingTextNodeIn(nodeAt(Seq(0)), 0, 0)
      } else {
        updateTempEmptyTextNodeIn(nodeChildArray(root), 0)
      }
    } else if (pos == rich.size) {
      rich.text.last match {
        case plain: Text.Plain =>
          updateExistingInsertingTextNodeIn(nodeAt(Seq(rich.text.size - 1)), plain.unicode.str.length, plain.unicode.size)
        case _ =>
          updateTempEmptyTextNodeIn(nodeChildArray(root), rich.text.size)
      }
    } else {
      val ss = rich.before(pos)
      val ee = rich.after(pos)
      ss match {
        case p: Atom.PlainGrapheme =>
          updateExistingInsertingTextNodeIn(nodeAt(ss.nodeCursor), ss.text.asPlain.unicode.toStringPosition(p.unicodeUntil), p.unicodeUntil)
        case s: Atom.SpecialOrMarked =>
          ee match {
            case es: Atom.SpecialOrMarked =>
              if (ss.nodeCursor.size < ee.nodeCursor.size) { // one wraps another
                updateTempEmptyTextNodeIn(nodeChildArray(nodeAt(ss.nodeCursor)), 0)
              } else if (ss.nodeCursor == ee.nodeCursor) { // same node, empty
                updateTempEmptyTextNodeIn(nodeChildArray(nodeAt(ss.nodeCursor)), 0)
              } else { // different sibling node
                updateTempEmptyTextNodeIn(nodeChildArray(nodeAt(model.cursor.Node.parent(ss.nodeCursor))), ss.nodeCursor.last + 1)
              }
            case ep: Atom.PlainGrapheme =>
              updateExistingInsertingTextNodeIn(nodeAt(ee.nodeCursor), 0, 0)
            case ec: Atom.CodedGrapheme =>
              updateExistingInsertingTextNodeIn(nodeAt(ee.nodeCursor), 0, 0)
            case _ =>
              throw new IllegalStateException("Not possible")
          }
        case c: Atom.CodedGrapheme =>
          val unicode = ss.text.asCoded.content
          ee match {
            case es: Atom.SpecialOrMarked =>
              updateExistingInsertingTextNodeIn(nodeAt(c.nodeCursor), unicode.toStringPosition(unicode.size), unicode.size)
            case ec: Atom.CodedGrapheme =>
              updateExistingInsertingTextNodeIn(nodeAt(c.nodeCursor), unicode.toStringPosition(ec.unicodeIndex), ec.unicodeIndex)
            case _ =>
              throw new IllegalStateException("Not possible")
          }
      }
    }
  }


  private def clearFormattedNodeHighlight(): Unit = {
    if (astHighlight != null) {
      astHighlight.classList.add("ct-ast-highlight")
      astHighlight = null
    }
  }

  private def addFormattedNodeHighlight(_5: HTMLSpanElement): Unit = {
    astHighlight = _5
    _5.classList.remove("ct-ast-highlight")
  }

  event(root, "compositionstart", (a: CompositionEvent) => {
    if (isInserting) controller.disableStateUpdate = true
    else preventDefault(a)
  })

  event(root, "compositionupdate", (a: CompositionEvent) => {
    if (!isInserting) preventDefault(a)
  })

  event(root, "compositionend", (a: CompositionEvent) => {
    // LATER Note that while every composition only has one compositionstart event, it may have several compositionend events.
    if (isInserting) controller.disableStateUpdate = false
    else preventDefault(a)
  })

  private def isSimpleInputType(inputType: String) = {
    inputType == "insertText" ||
      inputType == "insertFromComposition"
  }

  private def isOtherInputType(inputType: String) = {
    inputType == "insertReplacementText"
  }

  private def allowInputType(inputType: String) = {
    isSimpleInputType(inputType) || isOtherInputType(inputType)
  }

  // node, before
  private var replaceComplexInputBySimple: (raw.Text, String, Int) = null
  private var isComplexInput = false

  event("beforeinput", (a: Event) => {
    val ev = a.asInstanceOf[js.Dynamic]
    val inputType = ev.inputType.asInstanceOf[String]
    if (isSimpleInputType(inputType)) {
    } else if (isOtherInputType(inputType)) {
      val ranges = ev.getTargetRanges().asInstanceOf[js.Array[js.Dynamic]]
      if (ranges.length == 1) {
        val range = ranges(0)
        if (range.startContainer == range.endContainer && range.startContainer.isInstanceOf[raw.Text]) {
          val textNode = range.startContainer.asInstanceOf[raw.Text]
          val plainCursor = cursorOf(textNode)
          val indexOfPlain = contentData.content.indexOf(plainCursor)
          val textBefore = range.startContainer.textContent.asInstanceOf[String]
          replaceComplexInputBySimple =
            (textNode, textBefore, indexOfPlain)
        }
      }
      if (replaceComplexInputBySimple == null) {
        window.console.log(a)
        window.console.log(ev.getTargetRanges())
      }
    } else {
      if (a.cancelable) {
        window.console.log(a)
        a.preventDefault()
      }
    }
  })


  event("input", (a: Event) => {
    val ev = a.asInstanceOf[js.Dynamic]
    val inputType = ev.inputType.asInstanceOf[String]
    if (isSimpleInputType(inputType)) {
      controller.flush()
    } else if (isOtherInputType(inputType)) {
      if (replaceComplexInputBySimple != null) {
        removeInsertEmptyTextNode()
        insertNonEmptyTextNode = replaceComplexInputBySimple
        replaceComplexInputBySimple = null
      } else {
        window.console.log(a)
        isComplexInput = true
      }
      controller.flush()
    }
  })


  private def mergeTextsFix(center: raw.Text): String = {
    if (center.wholeText != center.textContent) {
      if (model.debug_view) {
//        println(s"whole text ${center.wholeText}")
//        println(s"text content ${center.textContent}")
      }
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

  private def flushComplex(): Unit = {
    throw new Exception("Cannot handle complex input yet, see log for details")
  }


//  def readSelection(): IntRange = {
//    //window.getSelection().anchorNode
//  }

  def readInsertionPoint(node: Node, pos: Int): Int = {
    if (window.getSelection().rangeCount == 1) {
      val range = window.getSelection().getRangeAt(0)
      if (range.collapsed) {
        if (range.startContainer == node) {
          val tc = node.textContent
          val cc = tc.codePointCount(0, range.startOffset)
          return cc + pos
        }
      }
    }
    -1
  }

  private def flushSimple(): Unit = {
    if (insertEmptyTextNode != null) {
      val (node, pos) = insertEmptyTextNode
      val tc = node.textContent
      assert(tc.startsWith(RichView.EvilChar))
      assert(tc.endsWith(RichView.EvilChar))
      val str = tc.substring(1, tc.length - 1)
      if (str.length > 0) {
        node.textContent = str
        insertNonEmptyTextNode = (node, str, pos)
        insertEmptyTextNode = null
        extraNode = null
        controller.onInsertRichTextAndViewUpdated(pos, pos, Unicode(str), readInsertionPoint(node, pos))
      }
    } else if (insertNonEmptyTextNode != null) {
      val (node, oldContent, pos) = insertNonEmptyTextNode
      val newContent = mergeTextsFix(node)
      val (from, to, text) = util.quickDiff(oldContent, newContent)
      val insertionPoint = readInsertionPoint(node, pos)
      if (model.debug_view) {
//        window.console.log(node)
//        window.console.log(node.parentNode)
//        println(s"old content $oldContent new content $newContent, $from, $to, $text, $insertionPoint")
      }
      if (from != to || !text.isEmpty) {
        insertNonEmptyTextNode = (node, newContent, pos)
        controller.onInsertRichTextAndViewUpdated(pos + from, pos + to, Unicode(text), insertionPoint)
        if (!isInserting) insertNonEmptyTextNode = null
      }
    }
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
    if (isComplexInput) {
      flushComplex()
      isComplexInput = false
    } else {
      flushSimple()
    }
  }

  private def clearInsertionMode(): Unit = {
    documentView.endInsertion()
    insertNonEmptyTextNode = null
    removeInsertEmptyTextNode()
  }

  private def clearVisualMode(): Unit = {
    clearRangeSelection()
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
    setRangeSelection(range, fromUser)
  }


  private def clearNormalMode(): Unit = {
    clearRangeSelection()
    clearFormattedNodeHighlight()
  }


  private def updateInsertMode(pos: Int, fromUser: Boolean): Unit = {
    val range = document.createRange()
    val start = updateInsertCursorAt(pos)
    range.setStart(start._1, start._2)
    range.setEnd(start._1, start._2)
    documentView.startInsertion(range)
    mergeTextsFix(start._1.asInstanceOf[raw.Text])
  }



  private def updateNormalMode(r: IntRange, fromUser: Boolean): Unit = {
    val (range, light) = nonEmptySelectionToDomRange(r)
    setRangeSelection(range, fromUser = fromUser)
    if (light != astHighlight) clearFormattedNodeHighlight()
    if (light != null) addFormattedNodeHighlight(light)
  }



  override def clearMode(): Unit = {
    clearEditor()
    initMode(if (isEmpty) -2 else -1)
    if (flushSubscription != null) {
      flushSubscription.cancel()
      flushSubscription = null
    }
  }

  private def isInserting = previousMode == 0

  private def initMode(i: Int): Unit = {
    if (previousMode < 0 && i >= 0) {
      if (flushSubscription == null) {
        flushSubscription = observe(controller.flushes.doOnNext(_ => {
          flushInsertionMode()
        }))
      }
    }
    if (previousMode != i) {
      if (debug_view) {
        println(s"mode change from  $previousMode to $i")
      }
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

  protected def clearEmptyNormalMode(): Unit = {
    clearRangeSelection()
    removeEmptyContent()
  }

  protected def initEmptyNormalMode(): Unit = {
    initEmptyContent()
    val range = document.createRange()
    range.setStart(root, 0)
    range.setEnd(root, 1)
    setRangeSelection(range, false)
  }

  var pmode: mode.Content.Rich = null

  override def updateMode(aa: mode.Content.Rich, viewUpdated: Boolean, editorUpdated: Boolean, fromUser: Boolean): Unit = {
    pmode = aa
    def updateViewMode(a: mode.Content.Rich, sub: Boolean): Unit = a match {
      case mode.Content.RichInsert(pos) =>
        if (!sub) {
          initMode(0)
          updateInsertMode(pos, fromUser)
        }
      case mode.Content.RichVisual(fix, move) =>
        if (!sub) initMode(1)
        updateVisualMode(fix, move, fromUser)
      case mode.Content.RichNormal(range) =>
        if (isEmpty) {
          if (!sub) initMode(3)
        } else {
          if (!sub) initMode(2)
          updateNormalMode(range, fromUser)
        }
    }
    if (fromUser) {
      scrollInToViewIfNotVisible(dom, documentView.dom)
    }
    aa match {
      case sub: mode.Content.RichSubMode =>
        updateViewMode(sub.modeBefore, true)
        sub match {
          case mode.Content.RichAttributeSubMode(range, mode) =>
            if (editor == null) {
              val text = sub.getText(rich)
              editor = documentView.attributeEditor
              val anchor = new UrlAttributeEditDialog.Anchor(controller) {
                override def rect: Rect = editorRect
              }
              documentView.attributeEditor.show(anchor, text.urlAttr, text.titleAttr)
           }
          case mode.Content.RichCodeSubMode(range, code, mode) =>
            if (editor == null) {
              editor = documentView.inlineEditor
              val text = sub.getText(rich)
              val anchor = new InlineCodeDialog.Anchor(controller, text.asCoded.content, code, text.asDelimited.delimitation.codeType) {
                override def rect: Rect = editorRect
              }
              documentView.inlineEditor.show(anchor)
            } else if (!editorUpdated) {
              documentView.inlineEditor.sync(code)
            }
        }
      case _ =>
        clearEditor()
        updateViewMode(aa, false)
    }
  }

  private var editor: Overlay = null

  def clearEditor(): Unit = {
    if (editor != null) {
      editor.dismiss()
      editor = null
    }
  }

  private def editorRect: Rect = {
    pmode match {
      case sub: mode.Content.RichSubMode =>
        val (sel, span) = nonEmptySelectionToDomRange(IntRange(sub.range.start - 1, sub.range.start))
        val rects = if (span == null) sel.getClientRects() else span.getClientRects()
        web.view.toRect(rects.item(0))
      case _ =>
        throw new IllegalStateException("Not possible!!!")
    }
  }

  private def setPreviousModeToEmpty(): Unit = {
    previousMode = if (isEmpty) -2 else -1
  }

  override def updateContent(data: model.data.Content.Rich, mode: Option[model.mode.Content.Rich], c: operation.Content.Rich, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    val dataBefore = rich
    contentView.updateContent(data, c, viewUpdated)
    if (!viewUpdated) {
      if (!editorUpdated) {
        if (editor != null) {
          editor match {
            case dialog: InlineCodeDialog =>
              mode match {
                case Some(model.mode.Content.RichCodeSubMode(range, code, modeBefore)) =>
                  dialog.sync(c.op.transformToCodeChange(IntRange(range.start, range.until)))
                  // no need to sync type change, this will dismiss the dialog instead
                case _ =>
              }
            case _ =>
          }
        }
      }
    }
  }
}
