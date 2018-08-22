package web.view.doc

import command.Key
import doc.{DocInterface, DocState}
import model.{cursor, data, range}
import model.data.{Node => _, _}
import model.range.IntRange
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{document, html, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.content._
import web.view._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class DocumentView(
  private val client: DocInterface,
  override protected val editor: EditorInterface,
  val windowRoot: HTMLElement
) extends EditorView {



  private val rootFrame = div(
    `class` := "ct-document-style ct-d-frame",
    width := "100%"
  ).render

  private val noEditable = div(
    position := "absolute",
    top := "-30px",
    width := "1000px",
    height := "0px",
    RichView.EvilChar).render


  private val nonEditableSelection = document.createRange()

  nonEditableSelection.setStart(noEditable.childNodes(0), 0)
  nonEditableSelection.setEnd(noEditable.childNodes(0), 1)

  val fakeSelections: Div = div(
    position := "absolute",
    width := "0px",
    height := "0px"
  ).render

  dom = div(
    position := "relative",
    `class` := "ct-scroll ct-document-view-root " + (if (!web.debug_fakeSelection) "ct-document-view-background" else ""),
    flex := "1 1 auto",
    paddingLeft := "36px",
    paddingRight := "36px",
    contenteditable := "true",
    overflowY := "scroll",
    fakeSelections,
    noEditable,
    div(height := "36px", display := "block", contenteditable := "false"),
    rootFrame,
    div(height := "36px", display := "block", contenteditable := "false")
  ).render

  private def cancelNoEditableInput(): Any = {
    window.setTimeout(() => {
      if (model.debug_view) println("cancelled illegal input")
      noEditable.textContent = RichView.EvilChar
      if (currentSelection == nonEditableSelection) {
        val sel = window.getSelection()
        if (sel.anchorNode != nonEditableSelection.startContainer) {
          sel.removeAllRanges()
          sel.addRange(nonEditableSelection)
        }
      }
    }, 100)
  }

  event(noEditable, "compositionstart", (a: CompositionEvent) => {
    cancelNoEditableInput()
  })

  event(noEditable, "compositionupdate", (a: CompositionEvent) => {
    cancelNoEditableInput()
  })

  event(noEditable, "compositionend", (a: CompositionEvent) => {
    cancelNoEditableInput()
  })

  event("beforeinput", (ev: Event) => {
    if (currentSelection == nonEditableSelection) {
      cancelNoEditableInput()
    }
  })

  /**
    *
    *
    *
    * state
    *
    *
    */
  private var isFocusedOut: Boolean = true
  private var duringStateUpdate: Boolean = false
  private var currentSelection: Range = nonEditableSelection


  event("blur", (a: FocusEvent) => {
    isFocusedOut = true
    dom.classList.add("ct-window-inactive")
  })

  override def focus(): Unit = {
    if (isFocusedOut) {
      isFocusedOut = false
      dom.classList.remove("ct-window-inactive")
      dom.focus()
      flushSelection()
    }
  }


  event("focus", (a: FocusEvent) => {
    if (!duringStateUpdate && isFocusedOut) {
      isFocusedOut = false
      dom.classList.remove("ct-window-inactive")
      flushSelection()
    }
  })


  private def flushSelection(): Unit = {
    if (!isFocusedOut) {
      val sel = window.getSelection()
      sel.removeAllRanges()
      sel.addRange(currentSelection)
    }
  }

  def startSelection(range: Range): Unit = {
    currentSelection = range
  }

  def endSelection(): Unit = {
    currentSelection = nonEditableSelection
  }

  def hasSelection: Boolean = {
    nonEditableSelection != currentSelection
  }

  def selection: Range = currentSelection

  private var activeContentEditor: ContentViewEditor.General = null

  private def activeContent =
    if (activeContentEditor == null || activeContentEditor.contentView.destroyed) null else activeContentEditor.contentView

  private def removeActiveContentEditor(): Unit = {
    if (activeContentEditor != null && !activeContentEditor.contentView.destroyed) {
      activeContentEditor.clearMode()
    }
    activeContentEditor = null
  }

  /**
    *
    * node list
    *
    *
    */
  //   frame = rootframe
  //     box
  //       content
  //       child list
  //         frame...
  //         frame...
  //         frame...
  //     hold
  //    nonEditable...

  private def inViewport(a: model.cursor.Node): Boolean = currentZoom != null && model.cursor.Node.contains(currentZoom, a)

  private def frameAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    if (rootFrame == this.rootFrame) assert(inViewport(at), s"not in viewport $at, current view port is $currentZoom")
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a
      else rec(a.childNodes(0).childNodes(1).childNodes(b.head), b.tail)
    }
    rec(rootFrame, at.drop(currentZoom.size)).asInstanceOf[HTMLElement]
  }

  private def boxAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    frameAt(at, rootFrame).childNodes(0).asInstanceOf[HTMLElement]
  }

  private def childListAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    boxAt(at, rootFrame).childNodes(1).asInstanceOf[HTMLElement]
  }

  private def frameInList(parent: HTMLElement, at: Int) = parent.childNodes(at).asInstanceOf[HTMLElement]


  private def holdAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    frameAt(at, rootFrame).childNodes(1).asInstanceOf[HTMLElement]
  }

  private def contentAt(at: model.cursor.Node, rootFrame: Node = rootFrame): ContentView.General = {
    val v = boxAt(at, rootFrame).childNodes(0).asInstanceOf[HTMLElement]
    View.fromDom[ContentView.General](v)
  }

  def cursorOf[T <: model.data.Content, O <: model.operation.Content](a: ContentView[T, O]): model.cursor.Node = {
    def rec(a: Node): Seq[Int] = {
      val frame = a.parentNode.parentNode
      if (frame == rootFrame) {
        currentZoom
      } else {
        val parent = frame.parentNode.childNodes
        var i = -1
        var j = 0
        while (i < 0 && j < parent.length) {
          if (frame == parent(j)) {
            i = j
          }
          j += 1
        }
        rec(frame.parentNode) :+ i
      }
    }
    rec(a.dom)
  }

  private var previousNodeVisual: ArrayBuffer[Element] = new ArrayBuffer[Element]()
  private var previousNodeMove: HTMLElement = null

  private def updateNodeVisual(v: model.mode.Node.Visual): Unit = {
    val newVisual = new ArrayBuffer[Element]()
    val overall = model.cursor.Node.minimalRange(v.fix, v.move)
    overall match {
      case None =>
        val rd = boxAt(currentZoom)
        newVisual.append(rd)
      case Some(range) => range.foreach(c => newVisual.append(boxAt(c)))
    }
    (newVisual -- previousNodeVisual).foreach(_.classList.add("ct-node-visual"))
    (previousNodeVisual -- newVisual).foreach(_.classList.remove("ct-node-visual"))
    previousNodeVisual = newVisual
    val newMove = boxAt(v.move)
    if (newMove != previousNodeMove) {
      if (previousNodeMove != null) previousNodeMove.classList.remove("ct-node-visual-move")
      previousNodeMove = newMove
      previousNodeMove.classList.add("ct-node-visual-move")
      if (v.move == currentZoom) {
        scrollToTop()
      } else {
        scrollInToViewIfNotVisible(previousNodeMove, dom)
      }
    }
  }

  private def clearNodeVisual(): Unit = {
    for (c <- previousNodeVisual) {
      c.classList.remove("ct-node-visual")
    }
    previousNodeVisual = ArrayBuffer.empty
    if (previousNodeMove != null) previousNodeMove.classList.remove("ct-node-visual-move")
    previousNodeMove = null
  }



  private def removeAllNodes(): Unit = {
    val a = rootFrame
    removeNodes(model.range.Node(currentZoom, IntRange(0, childListAt(currentZoom).childNodes.length)))
    contentAt(currentZoom).destroy()
    a.removeChild(a.childNodes(0))
    a.removeChild(a.childNodes(0))
  }

  private def removeNodes(range: model.range.Node): Unit = {
    def destroyContents(a: HTMLElement, start: Int, u: Int): Unit = {
      for (i <- start until u) {
        val frame = frameInList(a, i)
        val ll = childListAt(Seq.empty, rootFrame = frame)
        destroyContents(ll, 0, ll.children.length)
        contentAt(Seq.empty, rootFrame = frame).destroy()
      }
    }

    val p = childListAt(range.parent)
    destroyContents(p, range.childs.start, range.childs.until)
    for (_ <- range.childs) {
      // not using correct api here
      p.removeChild(p.children(range.childs.start))
    }
  }

  private def insertNodes(parentCur: model.cursor.Node, list: HTMLElement, at: Int, contents: Seq[model.data.Node]): Unit = {
    val before = if (at == list.childNodes.length) null else  list.childNodes.apply(at)
    contents.zipWithIndex.foreach(a => {
      val frame = div(`class` := "ct-d-frame").render
      list.insertBefore(frame, before)
      insertNodesRec(parentCur :+ a._2, a._1, frame)
    })
  }



  private def toggleHoldRendering(frame0: Node, hold0: Node, fold: Boolean): Unit = {
    val hold = hold0.asInstanceOf[HTMLElement]
    val frame = frame0.asInstanceOf[HTMLElement]
    if (fold) {
      if (!frame.classList.contains("ct-d-folded")) {
        frame.classList.add("ct-d-folded")
        hold.classList.add("ct-d-hold-folded")
      }
    } else {
      if (frame.classList.contains("ct-d-folded")) {
        frame.classList.remove("ct-d-folded")
        hold.classList.remove("ct-d-hold-folded")
      }
    }
  }




  // this can temp be null during state update
  private var currentZoom: model.cursor.Node = null
  private var currentZoomId: String = null

  private def insertNodesRec(cur: model.cursor.Node, root: model.data.Node, parent: html.Element): Unit = {
    val firstChild = parent.firstChild
    val box = div(`class` := classesFromNodeAttribute(root)).render
    parent.insertBefore(box, firstChild)
    val hold = tag("div")(contenteditable := "false", `class` := "ct-d-hold").render
    parent.insertBefore(hold, firstChild)
    createContent(root.content).attachToNode(box)
    val list = div(`class` := "ct-d-childlist").render
    // LATER mmm... this is a wired thing. can it be done more efficiently, like not creating the list at all?
    // LATER our doc transaction/fold handling is MESSY!!!
    hold.addEventListener("mouseover", handleHoverEvent)
    toggleHoldRendering(parent, hold, cur != currentZoom && client.state.userFoldedNodes.getOrElse(root.uuid, root.isH1))
    box.appendChild(list)
    insertNodes(cur, list, 0, root.childs)
  }


  private val handleHoverEvent: js.Function1[MouseEvent, Unit] = (e: MouseEvent) => {
    val hold = e.target.asInstanceOf[HTMLElement]
    val ee = View.fromDom[ContentView.General](hold.previousSibling.firstChild)
    val cur = cursorOf(ee)
    val node = client.state.node(cur)
    hold.title =
      Seq(
        node.attribute(model.data.Node.ContentType).map("content type: " + _.toString).getOrElse(""),
        node.attribute(model.data.Node.ChildrenType).map("children type: " + _.toString).getOrElse(""),
        s"items: ${node.count}",
        s"text size: ${node.content.size}",
        s"total text size: ${node.size}"
      ).filter(_.nonEmpty).mkString("\n")
  }


  private def createContent(c: Content): ContentView.General = {
    ContentView.create(c, true)
  }


  def selectionRect: Rect = {
    if (activeContent != null) {
      activeContentEditor.selectionRect
    } else if (previousNodeMove != null) {
      toRect(previousNodeMove.getBoundingClientRect())
    } else {
      toRect(dom.getBoundingClientRect())
    }
  }


  private def updateMode(m: Option[model.mode.Node], viewUpdated: Boolean = false, editorUpdated: Boolean = false, fromUser: Boolean = false): Unit = {
    duringStateUpdate = true
    m match {
      case None =>
        removeActiveContentEditor()
        clearNodeVisual()
      case Some(mk) => mk match {
        case model.mode.Node.Content(at, aa) =>
          clearNodeVisual()
          val current = contentAt(at)
          if (current != activeContent) {
            removeActiveContentEditor()
            activeContentEditor = current.createEditor(this, editor)
          }
          activeContentEditor.updateMode(aa, viewUpdated, editorUpdated, fromUser)
        case v@model.mode.Node.Visual(_, _) =>
          removeActiveContentEditor()
          updateNodeVisual(v)
      }
    }
    duringStateUpdate = false
    flushSelection()
  }

  // we use onAttach because we access window.setSelection
  override def onAttach(): Unit = {
    super.onAttach()
    val DocState(node, zoom, _, _, _) = client.state
    currentZoom = zoom
    currentZoomId = client.state.zoomId
    insertNodesRec(zoom, node(zoom), rootFrame)

    updateMode(client.state.mode)

    observe(client.stateUpdates.doOnNext(update => {
      update.foldsBefore.foreach(f => {
        if (inViewport(f._1)) toggleHoldRendering(frameAt(f._1), holdAt(f._1), f._2)
      })
      duringStateUpdate = true
      if (update.to.zoomId != currentZoomId) {
        //          if (cursor.Node.contains(currentZoom, a)) {
        //          } else {
        //            cleanFrame(rootFrame)
        //            insertNodesRec(update.root(a), rootFrame)
        //          }
        updateMode(None)
        removeAllNodes()
        currentZoomId = update.to.zoomId
        currentZoom = update.to.zoom
        insertNodesRec(currentZoom, update.to.node(currentZoom), rootFrame)
      } else {
        for ((s, t, to) <- update.from) {
          currentZoom = s.zoom
          currentZoomId = s.zoomId
          //              if (model.debug_view) {
          //                println(s"current zoom is $currentZoom")
          //                println(s"current trans is ${t._1}")
          //              }
          t match {
            case model.operation.Node.Content(at, c) =>
              val m: Option[model.mode.Content] = s.mode match {
                case Some(model.mode.Node.Content(at1, m)) if at == at1 => Some(m)
                case _ => None
              }
              if (inViewport(at)) {
                val content = contentAt(at)
                if (content == activeContent) {
                  activeContentEditor.updateContent(to.node(at).content, m, c, update.viewUpdated, update.editorUpdated)
                } else {
                  content.updateContent(to.node(at).content, c, update.viewUpdated)
                }
              }
            case model.operation.Node.AttributeChange(at, _, _) =>
              if (inViewport(at)) {
                val tt = classesFromNodeAttribute(to.node(at)).split(" ").filter(_.nonEmpty)
                val cl = boxAt(at).classList
                while (cl.length > 0) {
                  cl.remove(cl.item(0))
                }
                tt.foreach(cl.add)
                toggleHoldRendering(frameAt(at), holdAt(at), to.viewAsFolded(at))
              }
            case model.operation.Node.Replace(at, c) =>
              if (inViewport(at)) {
                val previousContent = contentAt(at)
                val p = previousContent.dom.parentNode
                val before = previousContent.dom.nextSibling
                previousContent.destroy()
                createContent(c).attachToNode(p, before.asInstanceOf[HTMLElement])
              }
            case model.operation.Node.Delete(r) =>
              if (inViewport(r.parent)) {
                removeNodes(r)
              }
            case model.operation.Node.Insert(at, childs) =>
              val pCur = model.cursor.Node.parent(at)
              if (inViewport(pCur)) {
                val root = childListAt(pCur)
                insertNodes(pCur, root, at.last, childs)
              }
            case model.operation.Node.Move(range, to) =>
              val toP = model.cursor.Node.parent(to)
              if (inViewport(range.parent) && inViewport(toP)) {
                val parent = childListAt(range.parent)
                val toParent = childListAt(toP)
                val nodes = range.childs.map(i => parent.childNodes.item(i)).toSeq
                val before = if (to.last < toParent.childNodes.length)  toParent.childNodes.item(to.last) else null
                nodes.foreach(n => {
                  toParent.insertBefore(n, before)
                })
              } else if (inViewport(range.parent)) {
                removeNodes(range)
              } else if (inViewport(to)) {
                val data = s.node(range)
                val p = model.cursor.Node.parent(to)
                val root = childListAt(p)
                insertNodes(p, root, to.last, data)
              }
          }
        }
      }
      currentZoom = update.to.zoom
      currentZoomId = update.to.zoomId
      duringStateUpdate = false
      updateMode(update.to.mode, update.viewUpdated, update.editorUpdated, update.fromUser)
      refreshMounted()
    }))
  }

  def refreshMounted(): Unit = {
    attributeEditor.refresh()
    inlineEditor.refresh()
    commandMenu.refresh()
  }

  event(window, "resize", (a: MouseEvent) => {
    refreshMounted()
    if (activeContent != null) activeContentEditor.refreshRangeSelection()
  })


  observe(editor.flushes.doOnNext(_ => if (activeContent != null) activeContentEditor.flush()))




  var sourceEditor: CoveringSourceEditDialog = null
  var commandMenu: CommandMenuDialog = null
  var attributeEditor: UrlAttributeEditDialog = null
  var inlineEditor : InlineCodeDialog = null




  private val commandMenuAnchor = new OverlayAnchor {
    override def rect: Rect = selectionRect
  }

  def showCommandMenu(): Unit = {
    commandMenu.show(commandMenuAnchor)
  }


  /**
    *
    *
    * mouse event currently disabled for entire document
    *
    */


  event("mousedown", (a: MouseEvent) => {
    //window.console.log(a)
  })

  event("mouseup", (a: MouseEvent) => {
    //window.console.log(a)
  })
  event("mousemove", (a: MouseEvent) => {
    //window.console.log(a)
    //preventDefault(a)
  })

  private var focusFinder: Int = -1

  // node, offset, n time, and handle
  private var selectionBeforeRead: (Node, Int, Int, Int) = null

  override def readSelectionOnKeyUpDown(): Unit = {
    clearAllPreviousReading()
    def inner(): Unit = {
      val cur = window.getSelection()
      def readNow(): Unit = {
        selectionBeforeRead = null
        focusAt(window.getSelection().anchorNode, 0, true)
      }
      if (selectionBeforeRead == null || selectionBeforeRead._3 < 10) {
        val nth = if (selectionBeforeRead == null) 0 else selectionBeforeRead._3 + 1
        selectionBeforeRead = (cur.anchorNode, cur.anchorOffset, nth, window.setTimeout(() => {
          if (selectionBeforeRead._1 != window.getSelection().anchorNode || selectionBeforeRead._2 !=  window.getSelection().anchorOffset) {
            readNow()
          } else {
            readSelectionOnKeyUpDown()
          }
        }, 1))
      } else {
        readNow()
      }
    }
    inner()
  }

  private def clearAllPreviousReading(): Unit = {
    if (focusFinder != -1) {
      window.clearTimeout(focusFinder)
      focusFinder = -1
    }
    if (selectionBeforeRead != null) {
      window.clearTimeout(selectionBeforeRead._4)
      selectionBeforeRead = null
    }
  }

  private def focusAt(t0: Node, delay: Int, isKeyboard: Boolean) = {
    clearAllPreviousReading()
    var t = t0
    while (t != null && t != dom) {
      View.maybeDom[ContentView.General](t) match {
        case Some(a) if a.isInstanceOf[ContentView.General] =>
          val contentView = a.asInstanceOf[ContentView.General]
          val cur = cursorOf(contentView)
          focusFinder = window.setTimeout(() => {
            val range = contentView.asInstanceOf[Any] match {
              case r: RichView =>
                window.console.log(t0)
                val sel = r.readSelectionFromDom()
                println(sel)
                sel
              case _ => None
            }
            editor.focusOn(cur, range, isKeyboard)
            focusFinder = -1
          }, delay)
          t = null
        case _ =>
      }
      if (t != null) t = t.parentNode
    }
    if (t != null) {
      editor.refreshMode()
    }
  }

  def onMouseInteract(a: MouseEvent, i: Int) = {
    focus()
    focusAt(a.target.asInstanceOf[Node], i, true)
  }

  event("click", (a: MouseEvent) => {
    window.console.log(a)
    onMouseInteract(a, 100)
  })

  event("dblclick", (a: MouseEvent) => {
    window.console.log(a)
    onMouseInteract(a, 5)
  })

  event("contextmenu", (a: MouseEvent) => {
    onMouseInteract(a, 5)
  })



  /**
    *
    *
    * drag drop currently disabled for entire document
    *
    *
    */

  event("dragstart", (a: DragEvent) => {
    preventDefault(a)
  })

  event("dragend", (a: DragEvent) => {
    preventDefault(a)
  })

  event("dragover", (a: DragEvent) => {
    preventDefault(a)
  })

  event("dragenter", (a: DragEvent) => {
    preventDefault(a)
  })

  event("drop", (a: DragEvent) => {
    preventDefault(a)
  })

}
