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

class EditableRichView(documentView: DocumentView, val controller: EditorInterface, rich0: Rich) extends RichView(rich0) with EditableContentView[model.data.Content.Rich, model.operation.Content.Rich, model.mode.Content.Rich]  {


  private var insertNonEmptyTextNode: (raw.Text, Int, Int) = null
  private var astHighlight: HTMLSpanElement = null
  private var flushSubscription: Cancelable = null
  private var previousMode = if (isEmpty) -2 else -1



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
                if (ee.text.isCodedNonAtomic) {
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

  private def clearSelection(): Unit = {
    val sel = window.getSelection
    sel.removeAllRanges()
  }

  private def setSelection(range: Range, fromUser: Boolean): Unit = {
    val sel = window.getSelection
    sel.removeAllRanges()
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
    removeEmptyContent()
  }

  protected def initEmptyNormalMode(): Unit = {
    initEmptyContent()
    val range = document.createRange()
    range.setStart(dom, 0)
    range.setEnd(dom, 1)
    setSelection(range, false)
  }

  override def updateMode(aa: mode.Content.Rich, viewUpdated: Boolean, fromUser: Boolean): Unit = {
    if (previousMode < 0) initMode()
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


  /**
    * will also remove from parent
    */
  override def destroy(): Unit = {
    clearMode()
    clearEditor()
    super.destroy()
  }

  private var editor: (Overlay, IntRange) = null

  def clearEditor(): Unit = {
    if (editor != null) {
      editor._1.dismiss()
      editor = null
    }
  }

  private def editorRect: Rect = {
    val (_, pos) = editor
    val (sel, span) = nonEmptySelectionToDomRange(IntRange(pos.start, pos.start + 1))
    val rects = if (span == null) sel.getClientRects() else span.getClientRects()
    web.view.toRect(rects.item(0))
  }


  def showLaTeXEditor(cur: model.cursor.Node, pos: range.IntRange, text: Unicode): Unit = {
    editor = (documentView.latexEditor, pos)
    val anchor = new LaTeXDialog.Anchor {
      override def rect: Rect = editorRect
      override def onDismiss(): Unit = editor = null
      override def update(url: Unicode): Unit = {
        controller.onLaTeXModified(documentView.cursorOf(EditableRichView.this), editor._2, url)
      }
    }
  }

  def showAttributeEditor(pos: IntRange, text: model.data.Text.Delimited): Unit = {
    editor = (documentView.attributeEditor, pos)
    val anchor = new UrlAttributeEditDialog.Anchor {
      override def rect: Rect = editorRect
      override def onDismiss(): Unit = editor = null
      override def update(url: Unicode, title: Unicode): Unit = {
        controller.onAttributeModified(documentView.cursorOf(EditableRichView.this), editor._2, url, title)
      }
    }
    documentView.attributeEditor.show(anchor, text.urlAttr.str, text.titleAttr.str)
  }


  override def updateContent(data: model.data.Content.Rich, c: operation.Content.Rich, viewUpdated: Boolean): Unit = {
    rich = data.content
    clearMode()
    updateContent(c, viewUpdated)
    if (!viewUpdated) {
      // val cs = c.asInstanceOf[operation.Content.Rich]
      // TODO incrementally update dom remember to clear the empty range when needed
      if (editor != null) {

        val range = editor._2
        val fakeMode = model.mode.Content.RichVisual(IntRange(range.start), IntRange(range.until - 1))
        val res = c.transform(data, fakeMode)._1
        res match {
          case model.mode.Content.RichVisual(a, b) =>
            editor = (editor._1, IntRange(a.start, b.until))
          case _ => clearEditor()
        }
      }
    }
  }
}
