package web.ui.content

import model._
import model.data._
import model.mode.Content
import model.mode.Content.RichVisual
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom
import org.scalajs.dom.html.Paragraph
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLDivElement, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{ClientRect, document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.{EditorInterface, RichEditInterface}
import web.ui
import web.ui.doc.DocumentView
import web.view.{Overlay, _}
import web.ui.dialog.{InlineCodeDialog, UrlAttributeEditDialog}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class RichViewEditor(val documentView: DocumentView, val controller: RichEditInterface, override val contentView: RichView) extends ContentViewEditor[model.data.Content.Rich, model.operation.Content.Rich, model.mode.Content.Rich](contentView)  {


  /**
    * selection
    */

  private var fakeRangeGlowing: Boolean = false
  private var fakeRangeSelection: Range = null

  override def selectionRect: Rect = {
    web.view.toRect(if (fakeRangeSelection != null) {
      fakeRangeSelection.getBoundingClientRect()
    } else if (pmode.isInstanceOf[model.mode.Content.RichCodeSubMode]) {
      val atom = contentView.rich.after(pmode.asInstanceOf[model.mode.Content.RichCodeSubMode].range.start - 1)
      contentView.nodeAt(atom.nodeCursor).asInstanceOf[HTMLElement].getBoundingClientRect()
    } else if (documentView.hasSelection) {
      documentView.selection.getBoundingClientRect()
    } else {
      contentView.dom.getBoundingClientRect()
    })
  }

  override def refreshRangeSelection(): Unit = {
    if (fakeRangeSelection != null) {
      setRangeSelection(fakeRangeSelection, false)
    }
  }


  private def clearRangeSelection(): Unit = {
    if (web.debug_fakeSelection) {
      if (fakeRangeSelection != null) {
        removeAllChild(documentView.fakeSelections)
      }
      fakeRangeSelection = null
    } else {
      documentView.endSelection()
    }
  }

  private def setRangeSelection(range: Range, fromUser: Boolean): Unit = {
    if (web.debug_fakeSelection) {
      clearRangeSelection()
      val dom = documentView.fakeSelections
      val p = dom.getBoundingClientRect()
      fakeRangeSelection = range

      val ar = contentView.linesFromClientRects(range.getClientRects())
      for (rect <- ar.reverse) {
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
    } else {
      documentView.startSelection(range)
    }
  }



  import contentView._

  private var insertEmptyTextNode: (raw.Text, Int) = null
  private var insertNonEmptyTextNode: (raw.Text, String, Int) = null
  private var astHighlight: HTMLSpanElement = null



  def removeInsertEmptyTextNode(): Unit = {
    if (extraNode != null) {
      removeFromParent(extraNode)
      extraNode = null
      assert(insertEmptyTextNode != null)
      insertEmptyTextNode = null
    } else {
      assert(insertEmptyTextNode == null)
    }
  }

  defer(_ => {
    clearMode()
  })

  private def updateInsertCursorAt(pos: Int): (Node, Int) = {

    def createTempEmptyInsertTextNode(node: Node, i: Int, pos: Int): Unit = {
      val extra = document.createTextNode(s"${ui.EvilChar}${ui.EvilChar}")
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
    val (node, offset, unicode) = posInDom(pos)
    if (unicode == -1) {
      if (offset > 0) {
        node.childNodes(offset - 1) match {
          case t: raw.Text =>
            // LATER try not count it
            return updateExistingInsertingTextNodeIn(t, t.textContent.length, t.textContent.codePointCount(0, t.textContent.length))
          case _ =>
        }
      }
      if (offset < node.childNodes.length) {
        node.childNodes(offset) match {
          case t: raw.Text =>
            return updateExistingInsertingTextNodeIn(t, 0, 0)
          case _ =>
        }
      }
      updateTempEmptyTextNodeIn(node, offset)
    } else {
      updateExistingInsertingTextNodeIn(node, offset, unicode)
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

  private def isCompositionInputType(inputType: String) = {
    inputType == "insertCompositionText"
  }

  private def isSimpleInputType(inputType: String) = {
    inputType == "insertText" ||
      inputType == "insertFromComposition"
  }

  private def isReplacementInputType(inputType: String) = {
    inputType == "insertReplacementText"
  }

  private def allowInputType(inputType: String) = {
    isSimpleInputType(inputType) || isReplacementInputType(inputType)
  }

  // node, before
  private var pendingFlush = false
  private var goBackToNormalAfterFlush = false
  private var domModifiedInBeforeEvent = false

  override def beforeInputEvent(a: Event): Unit = {
    if (previousMode == 2 || previousMode == 3) {
      // don't allow input in normal mode
      a.preventDefault()
    } else {
      pendingFlush = true
      goBackToNormalAfterFlush = previousMode == 1 && controller.enableModal
      domModifiedInBeforeEvent = previousMode == 1 && controller.onDeleteCurrentSelectionAndStartInsert()
    }
  }


  override def compositionEndEvent(): Unit = {
    pendingFlush = true
    flush()
  }

  override def inputEvent(a: Event): Unit = {
    if (pendingFlush) {
      if (!isCompositionInputType(a.asInstanceOf[js.Dynamic].inputType.asInstanceOf[String])) {
        flush()
      } else {
        pendingFlush = false
      }
    } else {
      if (model.debug_view) window.console.log("unknown input event", a)
      refreshDom()
    }
  }

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

  override def flush(): Unit = {
    if (!pendingFlush) return

    pendingFlush = false

    if (insertEmptyTextNode != null) {
      val (node, pos) = insertEmptyTextNode
      val tc = node.textContent
      assert(tc.startsWith(ui.EvilChar))
      assert(tc.endsWith(ui.EvilChar))
      val str = tc.substring(1, tc.length - 1)
      if (str.length > 0) {
        node.textContent = str
        insertNonEmptyTextNode = (node, str, pos)
        insertEmptyTextNode = null
        extraNode = null
        controller.onInsertRichTextAndViewUpdated(pos, pos, Unicode(str), goBackToNormalAfterFlush, -1, domModifiedInBeforeEvent)
      } else if (goBackToNormalAfterFlush) {
        removeInsertEmptyTextNode()
      }
    } else if (insertNonEmptyTextNode != null) {
      val (node, oldContent, pos) = insertNonEmptyTextNode
      val newContent = mergeTextsFix(node)
      val (from0, to0, text) = util.quickDiff(oldContent, newContent)
      val from = oldContent.codePointCount(0, from0)
      val to = oldContent.codePointCount(from0, to0) + from
      val insertionPoint = if (domModifiedInBeforeEvent) -1 else readPlainInsertionPointBeforeFlush()
      if (from != to || !text.isEmpty) {
        if (model.debug_view) {
             window.console.log(node)
             window.console.log(node.parentNode)
             println(s"old content $oldContent new content $newContent, $from, $to, $text, $insertionPoint")
        }
        insertNonEmptyTextNode = (node, newContent, pos)
        val mode = controller.onInsertRichTextAndViewUpdated(pos + from, pos + to, Unicode(text), goBackToNormalAfterFlush, insertionPoint, domModifiedInBeforeEvent)
        mode match {
          case model.mode.Content.RichInsert(i) if insertionPoint == i =>
          case _ =>
            updateMode(mode, false, false, false)
        }
      }
      if (goBackToNormalAfterFlush) {
        insertNonEmptyTextNode = null
      }
    }
  }


  private def clearInsertionMode(): Unit = {
    documentView.endSelection()
    insertNonEmptyTextNode = null
    removeInsertEmptyTextNode()
  }

  private def clearVisualMode(): Unit = {
    clearRangeSelection()
  }

  private def updateVisualMode(fix: IntRange, move: IntRange, fromUser: Boolean): Unit = {
    fakeRangeGlowing = true
    val (r1,_) = nonEmptySelectionToDomRange(fix.merge(move))
    setRangeSelection(r1, fromUser)
  }


  private def clearNormalMode(): Unit = {
    fakeRangeGlowing = false
    clearRangeSelection()
    clearFormattedNodeHighlight()
  }


  private def updateInsertMode(pos: Int, fromUser: Boolean): Unit = {
    val range = document.createRange()
    val start = updateInsertCursorAt(pos)
    range.setStart(start._1, start._2)
    range.setEnd(start._1, start._2)
    documentView.startSelection(range)
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
  }

  private def isInserting = previousMode == 0

  private def initMode(i: Int): Unit = {
    if (previousMode < 0 && i >= 0) {
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
      if (i == -2) {
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
    def updateViewMode(a: mode.Content.Rich, sub: Boolean): Unit = {
      a match {
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
            if (!sub) {
              initMode(3)
              initEmptyNormalMode()
            }
          } else {
            if (!sub) initMode(2)
            updateNormalMode(range, fromUser)
          }
        case _ => throw new IllegalStateException("Not here!")
      }
    }
    if (fromUser) {
      scrollInToViewIfNotVisible(contentView.dom, documentView.dom)
    }
    aa match {
      case sub: mode.Content.RichSubMode =>
        updateViewMode(sub.modeBefore, false)
        sub match {
          case mode.Content.RichAttributeSubMode(range, mode) =>
            if (editor == null) {
              val text = sub.getText(rich)
              editor = documentView.attributeEditor
              val anchor = new UrlAttributeEditDialog.Anchor(controller) {
                override def rect: Rect = selectionRect
              }
              documentView.attributeEditor.show(anchor, text.urlAttr, text.titleAttr)
           }
          case mode.Content.RichCodeSubMode(range, code, mode) =>
            if (editor == null) {
              editor = documentView.inlineEditor
              val text = sub.getText(rich)
              val anchor = new InlineCodeDialog.Anchor(text.asCoded.content, code, text.asDelimited.delimitation.codeType) {
                override def rect: Rect = selectionRect
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

  override def updateContent(data: model.data.Content.Rich, mode: Option[model.mode.Content.Rich], c: operation.Content.Rich, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    contentView.updateContent(data, c, viewUpdated)
    if (!viewUpdated) {
      if (!editorUpdated) {
        if (editor != null) {
          editor match {
            case dialog: InlineCodeDialog =>
              mode match {
                case Some(model.mode.Content.RichCodeSubMode(range, code, modeBefore)) =>
                  c.op.transformToCodeChange(IntRange(range.start, range.until)) match {
                    case Some(edit) =>
                      dialog.sync(edit)
                    case _ => clearEditor()
                  }
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
