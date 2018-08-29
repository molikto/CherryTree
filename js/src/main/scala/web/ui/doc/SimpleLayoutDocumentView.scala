package web.ui.doc

import command.Key
import doc.{DocInterface, DocState}
import model.data.Node.ContentType
import model.{cursor, data, range}
import model.data.{Node => _, _}
import model.mode.Content.RichInsert
import model.range.IntRange
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{document, html, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.ui
import web.ui.content._
import web.view.{OverlayAnchor, _}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class SimpleLayoutDocumentView(
  override protected val client: DocInterface,
  override protected val editor: EditorInterface
) extends DocumentView with EditorView with DocFramer {


  private val rootFrame = div(
    `class` := "ct-document-style ct-d-frame",
    width := "100%"
  ).render

  dom.appendChild(div(`class` := "unselectable", height := "36px",  display := "block", contenteditable := "false").render)
  dom.appendChild(rootFrame)
  dom.appendChild(div(`class` := "unselectable", height := "36px",  display := "block", contenteditable := "false").render)

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

  private def frameAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    if (rootFrame == this.rootFrame) assert(currentDoc == null || currentDoc.inViewport(at), s"not in viewport $at, current view port is $currentZoom")
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

  override protected def contentOfHold(a: Node): ContentView.General = View.fromDom(a.previousSibling.firstChild)

  override protected def contentAt(at: model.cursor.Node): ContentView.General = contentAt(at, rootFrame)

  private def contentAt(at: model.cursor.Node, rootFrame: Node): ContentView.General = {
    val v = boxAt(at, rootFrame).childNodes(0).asInstanceOf[HTMLElement]
    View.fromDom[ContentView.General](v)
  }

  override protected  def cursorOf[T <: model.data.Content, O <: model.operation.Content](a: ContentView[T, O]): model.cursor.Node = {
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

  private def updateNodeVisual(v: model.mode.Node.Visual, fromUser: Boolean): Unit = {
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
      if (fromUser && !duringMouseMovement) { // don't scroll for mouse
        if (v.move == currentZoom) {
          scrollToTop()
        } else {
          scrollInToViewIfNotVisible(previousNodeMove, dom)
        }
      }
    }
  }

  private def clearNodeVisual(): Unit = {
    if (previousNodeMove != null) {
      for (c <- previousNodeVisual) {
        c.classList.remove("ct-node-visual")
      }
      previousNodeVisual = ArrayBuffer.empty
      if (previousNodeMove != null) previousNodeMove.classList.remove("ct-node-visual-move")
      previousNodeMove = null
    }
  }



  private def removeAllNodes(): Unit = {
    val a = rootFrame
    removeNodes(model.range.Node(currentZoom, IntRange(0, childListAt(currentZoom).childNodes.length)))
    contentAt(currentZoom).destroy()
    a.removeChild(a.childNodes(0))
    a.removeChild(a.childNodes(0))
  }

  private def destroyContents(a: HTMLElement, start: Int, u: Int): Unit = {
    for (i <- start until u) {
      val frame = frameInList(a, i)
      val ll = childListAt(Seq.empty, rootFrame = frame)
      destroyContents(ll, 0, ll.children.length)
      contentAt(Seq.empty, rootFrame = frame).destroy()
    }
  }

  private def removeNodes(range: model.range.Node): Unit = {

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
      insertNodeRec(parentCur :+ a._2, a._1, frame)
    })
  }



  private def toggleHoldRendering(cur: model.cursor.Node, frame0: Node, childlist: Node, hold0: Node, fold: Boolean): Unit = {
    val hold = hold0.asInstanceOf[HTMLElement]
    val frame = frame0.asInstanceOf[HTMLElement]
    val cl = if (childlist == null) null else childlist.asInstanceOf[HTMLElement]
    if (fold) {
      if (!frame.classList.contains("ct-d-folded")) {
        frame.classList.add("ct-d-folded")
        hold.classList.add("ct-d-hold-folded")
        if (cl != null) {
          destroyContents(cl, 0, cl.childNodes.length)
          removeAllChild(cl)
        }
      }
    } else {
      if (frame.classList.contains("ct-d-folded")) {
        frame.classList.remove("ct-d-folded")
        hold.classList.remove("ct-d-hold-folded")
        if (cl != null) insertNodes(cur, cl, 0, currentDoc.node(cur).childs)
      }
    }
  }




  // this can temp be null during state update
  private var currentDoc: DocState = null
  private def currentZoom: model.cursor.Node = currentDoc.zoom
  private def currentZoomId: String = currentDoc.zoomId

  private def insertNodeRec(cur: model.cursor.Node, root: model.data.Node, parent: html.Element): Unit = {
    val firstChild = parent.firstChild
    val box = div(`class` := classesFromNodeAttribute(root)).render
    parent.insertBefore(box, firstChild)
    val hold = tag("div")(contenteditable := "false", `class` := "ct-d-hold").render
    parent.insertBefore(hold, firstChild)
    createContent(root.content, root.contentType).attachToNode(box)
    val list = div(`class` := "ct-d-childlist").render
    // LATER mmm... this is a wired thing. can it be done more efficiently, like not creating the list at all?
    // LATER our doc transaction/fold handling is MESSY!!!
    hold.addEventListener("mouseover", handleHoverEvent)
    box.appendChild(list)
    val folded = currentDoc.viewAsFolded(root)
    toggleHoldRendering(cur, parent, null, hold, folded)
    if (!folded) insertNodes(cur, list, 0, root.childs)
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

  def simulateKeyboardMotion(isUp: Boolean): Unit = {
  }


  private def updateMode(m: Option[model.mode.Node], viewUpdated: Boolean = false, editorUpdated: Boolean = false, fromUser: Boolean = false): Unit = {
    duringStateUpdate = true
    m match {
      case None =>
        allowCompositionInput = false
        removeActiveContentEditor()
        endSelection()
        clearNodeVisual()
      case Some(mk) => mk match {
        case model.mode.Node.Content(at, aa) =>
          allowCompositionInput = aa match {
            case _: model.mode.Content.RichInsert => true
            case _: model.mode.Content.RichVisual => true
            case _ => false
          }
          clearNodeVisual()
          val current = contentAt(at)
          if (current != activeContent) {
            removeActiveContentEditor()
            activeContentEditor = current.createEditor(this, editor)
          }
          activeContentEditor.updateMode(aa, viewUpdated, editorUpdated, fromUser)
        case v@model.mode.Node.Visual(_, _) =>
          allowCompositionInput = false
          removeActiveContentEditor()
          endSelection()
          updateNodeVisual(v, fromUser)
      }
    }
    duringStateUpdate = false
    flushSelection()
  }


  // we use onAttach because we access window.setSelection
  override def onAttach(): Unit = {
    super.onAttach()
    val DocState(node, zoom, _, _, _) = client.state
    currentDoc = client.state
    insertNodeRec(zoom, currentDoc.node(currentDoc.zoom), rootFrame)

    updateMode(client.state.mode)

    observe(client.stateUpdates.doOnNext(update => {
      update.foldsBefore.foreach(f => {
        val fr = frameAt(f._1)
        if (currentDoc.visible(f._1)) toggleHoldRendering(f._1, fr, childListAt(Seq.empty, fr), holdAt(Seq.empty, fr), f._2)
      })
      duringStateUpdate = true
      if (update.to.zoomId != currentZoomId) {
        //          if (cursor.Node.contains(currentZoom, a)) {
        //          } else {
        //            cleanFrame(rootFrame)
        //            insertNodeRec(update.root(a), rootFrame)
        //          }
        updateMode(None)
        removeAllNodes()
        currentDoc = update.to
        insertNodeRec(currentZoom, update.to.node(currentZoom), rootFrame)
        scrollToTop()
      } else {
        for ((s, t, to) <- update.from) {
          currentDoc = s
          //              if (model.debug_view) {
          //                println(s"current zoom is $currentZoom")
          //                println(s"current trans is ${t._1}")
          //              }
          def replaceContent(at: model.cursor.Node, c: Content, contentType: Option[ContentType]) = {
            val previousContent = contentAt(at)
            val p = previousContent.dom.parentNode
            val before = previousContent.dom.nextSibling
            previousContent.destroy()
            createContent(c, contentType).attachToNode(p, before.asInstanceOf[HTMLElement])
          }

          t match {
            case model.operation.Node.Content(at, c) =>
              val m: Option[model.mode.Content] = s.mode match {
                case Some(model.mode.Node.Content(at1, m)) if at == at1 => Some(m)
                case _ => None
              }
              if (s.visible(at)) {
                val content = contentAt(at)
                if (content == activeContent) {
                  activeContentEditor.updateContent(to.node(at).content, m, c, update.viewUpdated, update.editorUpdated)
                } else {
                  content.updateContent(to.node(at).content, c, update.viewUpdated)
                }
              }
            case model.operation.Node.AttributeChange(at, _, _) =>
              if (s.visible(at)) {
                val old = to.node(at)
                if (!ContentView.matches(old.content, old.contentType, contentAt(at))) {
                  replaceContent(at, old.content, to.node(at).contentType)
                }
                boxAt(at).className = classesFromNodeAttribute(to.node(at))
                val fr = frameAt(at)
                toggleHoldRendering(at, fr, childListAt(Seq.empty, fr), holdAt(Seq.empty, fr), to.viewAsFolded(at))
              }
            case model.operation.Node.Replace(at, c) =>
              if (s.visible(at)) {
                replaceContent(at, c, to.node(at).contentType)
              }
            case model.operation.Node.Delete(r) =>
              if (s.viewAsNotFoldedAndNotHidden(r.parent)) {
                removeNodes(r)
              }
            case model.operation.Node.Insert(at, childs) =>
              val pCur = model.cursor.Node.parent(at)
              if (s.visible(at)) {
                val root = childListAt(pCur)
                insertNodes(pCur, root, at.last, childs)
              }
            case model.operation.Node.Move(range, to) =>
              val toP = model.cursor.Node.parent(to)
              if (s.viewAsNotFoldedAndNotHidden(range.parent) && s.visible(to)) {
                val parent = childListAt(range.parent)
                val toParent = childListAt(toP)
                val nodes = range.childs.map(i => parent.childNodes.item(i)).toSeq
                val before = if (to.last < toParent.childNodes.length)  toParent.childNodes.item(to.last) else null
                nodes.foreach(n => {
                  toParent.insertBefore(n, before)
                })
              } else if (s.viewAsNotFoldedAndNotHidden(range.parent)) {
                removeNodes(range)
              } else if (s.visible(to)) {
                val data = s.node(range)
                val p = model.cursor.Node.parent(to)
                val root = childListAt(p)
                insertNodes(p, root, to.last, data)
              }
          }
        }
      }
      currentDoc = update.to
      duringStateUpdate = false
      updateMode(update.to.mode, update.viewUpdated, update.editorUpdated, update.fromUser)
      refreshMounted()
    }))
  }

  def scrollToTop(cur: model.cursor.Node): Unit = {
    contentAt(cur).dom.scrollIntoView(true)
    dom.scrollTop  = Math.max(0, dom.scrollTop - 10)
  }

}
