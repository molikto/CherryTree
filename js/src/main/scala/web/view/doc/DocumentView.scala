package web.view.doc

import command.Key
import doc.{DocInterface, DocState}
import model.{cursor, data, range}
import model.data.Content
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{document, html, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.content.{ContentView, RichView, SourceView}
import web.view._

import scala.collection.mutable.ArrayBuffer

class DocumentView(
  private val client: DocInterface,
  override protected val editor: EditorInterface
) extends EditorView {


  private val rootFrame = div(
    `class` := "ct-root ct-d-frame",
    width := "100%"
  ).render

  private val noEditable = div(contenteditable := true, width := "0px", height := "0px").render
  dom = div(
    `class` := "ct-scroll",
    flex := "1 1 auto",
    paddingLeft := "36px",
    paddingRight := "36px",
    overflowY := "scroll",
    div(height := "36px", display := "block"),
    rootFrame,
    div(height := "36px", display := "block"),
    noEditable,
    color := theme.contentText
  ).render

  /**
    *
    *
    *
    * state
    *
    *
    */
  private var currentEditable: HTMLElement = noEditable
  private var isFocusedOut: Boolean = false
  private var duringStateUpdate: Boolean = false


  event("focusout", (a: FocusEvent) => {
    a.relatedTarget match {
      case h: HTMLElement if dom.contains(h) =>
      case _ =>
        if (!duringStateUpdate) {
          isFocusedOut = true
          dom.classList.add("ct-window-inactive")
        }
    }
  })

  override def focus(): Unit = {
    isFocusedOut = false
    dom.classList.remove("ct-window-inactive")
    if (document.activeElement != currentEditable) {
      if (currentEditable != noEditable) {
        currentEditable = null
        updateMode(None, viewUpdated = false)
        updateMode(client.state.mode, viewUpdated = false)
      } else {
        noEditable.focus()
      }
    }
  }


  event("focusin", (a: FocusEvent) => {
    if (!duringStateUpdate && isFocusedOut) {
      isFocusedOut = false
      dom.classList.remove("ct-window-inactive")
    }
  })


  override def markEditable(dom: HTMLElement): Unit = {
    if (currentEditable == dom) return
    dom.contentEditable = "true"
    noEditable.contentEditable = "false"
    currentEditable = dom
    currentEditable.focus()
  }

  override def unmarkEditable(dom: HTMLElement): Unit = {
    if (dom == currentEditable) {
      dom.contentEditable = "false"
      currentEditable = noEditable
      noEditable.contentEditable = "true"
      noEditable.focus()
    }
  }

  private var focusContent: ContentView.General = null

  private def removeFocusContent(): Unit = {
    if (focusContent != null) {
      if (!focusContent.destroyed) focusContent.clearMode()
      focusContent = null
    }
  }

  /**
    *
    * node list
    *
    *
    */
  //   frame = rootframe
  //     hold
  //     box
  //       content
  //       child list
  //         frame...
  //         frame...
  //         frame...
  //    nonEditable...

  private def inViewport(a: model.cursor.Node): Boolean = currentZoom != null && a.startsWith(currentZoom)

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

  def cursorOf[T <: model.data.Content, O <: model.operation.Content, M <: model.mode.Content](a: ContentView[T, O, M]): model.cursor.Node = {
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

  private def classesFromNodeAttribute(node: model.data.Node): String = {
    node.attribute(model.data.Node.ContentType).map {
      case model.data.Node.ContentType.Cite => "ct-d-cite"
      case model.data.Node.ContentType.Br => "ct-d-br"
      case model.data.Node.ContentType.Heading(j) => if (j > 1) s"ct-d-heading ct-d-h$j" else s"ct-d-h$j"
      case _ => ""
    }.getOrElse("") + " " + node.attribute(model.data.Node.ChildrenType).map {
      case model.data.Node.ChildrenType.UnorderedList => "ct-d-ul"
      case model.data.Node.ChildrenType.OrderedList => "ct-d-ol"
      case model.data.Node.ChildrenType.Paragraphs => "ct-d-ps"
      case model.data.Node.ChildrenType.DashList => "ct-d-dl"
      case _ => ""
    }.getOrElse("")
  }


  private def cleanFrame(a: html.Element): Unit = {
    a.removeChild(a.childNodes(0))
    a.removeChild(a.childNodes(0))
  }

  // this can temp be null during state update
  private var currentZoom: model.cursor.Node = null
  private var currentZoomId: String = null

  private def insertNodesRec(cur: model.cursor.Node, root: model.data.Node, parent: html.Element): Unit = {
    val firstChild = parent.firstChild
    val box = div(`class` := "ct-d-box " + classesFromNodeAttribute(root)).render
    parent.insertBefore(box, firstChild)
    val hold = tag("i")(`class` := "ct-d-hold").render
    parent.insertBefore(hold, firstChild)
    createContent(root.content).attachToNode(box)
    val list = div(`class` := "ct-d-childlist").render
    // LATER mmm... this is a wired thing. can it be done more efficiently, like not creating the list at all?
    // LATER our doc transaction/fold handling is MESSY!!!
    toggleHoldRendering(parent, hold, cur != currentZoom && client.state.userFoldedNodes.getOrElse(root.uuid, root.isH1))
    box.appendChild(list)
    insertNodes(cur, list, 0, root.childs)
  }


  private def createContent(c: Content): ContentView.General = {
    c match {
      case model.data.Content.Rich(cs) =>
        new RichView(this, editor, cs).asInstanceOf[ContentView.General]
      case c@model.data.Content.Code(_, _) =>
        new SourceView(this, editor, c).asInstanceOf[ContentView.General]
    }
  }


  def selectionRect: Rect = {
    if (focusContent != null) {
      focusContent.selectionRect
    } else if (previousNodeMove != null) {
      toRect(previousNodeMove.getBoundingClientRect())
    } else {
      toRect(dom.getBoundingClientRect())
    }
  }


  private def updateMode(m: Option[model.mode.Node], viewUpdated: Boolean, fromUser: Boolean = false): Unit = {
    duringStateUpdate = true
    m match {
      case None =>
        removeFocusContent()
        clearNodeVisual()
      case Some(mk) => mk match {
        case model.mode.Node.Content(at, aa) =>
          clearNodeVisual()
          val current = contentAt(at)
          if (current != focusContent) {
            removeFocusContent()
            current.initMode()
            focusContent = current
          }
          aa match {
            case r: model.mode.Content.Rich =>
              current.asInstanceOf[RichView].updateMode(r, viewUpdated, fromUser)
            case c: model.mode.Content.Code =>
              current.asInstanceOf[SourceView].updateMode(c, viewUpdated, fromUser)
          }
        case v@model.mode.Node.Visual(_, _) =>
          removeFocusContent()
          updateNodeVisual(v)
      }
    }
    duringStateUpdate = false
  }

  // we use onAttach because we access window.setSelection
  override def onAttach(): Unit = {
    super.onAttach()
    val DocState(node, zoom, _, _, _) = client.state
    currentZoom = zoom
    currentZoomId = client.state.zoomId
    insertNodesRec(zoom, node(zoom), rootFrame)

    updateMode(client.state.mode, viewUpdated = false)

    observe(client.stateUpdates.doOnNext(update => {
      update.foldsBefore.foreach(f => {
        if (inViewport(f._1)) toggleHoldRendering(frameAt(f._1), holdAt(f._1), f._2)
      })
      duringStateUpdate = true
      if (update.to.zoomId != currentZoomId) {
        currentZoomId = update.to.zoomId
        currentZoom = update.to.zoom
        //          if (currentZoom.startsWith(a)) {
        //          } else {
        //            cleanFrame(rootFrame)
        //            insertNodesRec(update.root(a), rootFrame)
        //          }
        updateMode(None, false, false)
        cleanFrame(rootFrame)
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
              if (inViewport(at)) {
                contentAt(at).updateContent(to.node(at).content, c, update.viewUpdated)
              }
            case model.operation.Node.AttributeChange(at, _, _) =>
              if (inViewport(at)) {
                val tt = classesFromNodeAttribute(to.node(at)).split(" ").filter(_.nonEmpty) :+ "ct-d-box"
                val cl = boxAt(at).classList
                while (cl.length > 0) {
                  cl.remove(cl.item(0))
                }
                tt.foreach(cl.add)
                toggleHoldRendering(frameAt(at), holdAt(at), to.viewAsFolded(at))
                updateMode(None, viewUpdated = false)
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
              val pCur = at.dropRight(1)
              if (inViewport(pCur)) {
                val root = childListAt(pCur)
                insertNodes(pCur, root, at.last, childs)
              }
            case model.operation.Node.Move(range, to) =>
              val toP = to.dropRight(1)
              if (inViewport(range.parent) && inViewport(toP)) {
                val parent = childListAt(range.parent)
                val toParent = childListAt(toP)
                val nodes = range.childs.map(i => parent.childNodes.item(i)).toSeq
                val before = if (to.last < toParent.childNodes.length)  toParent.childNodes.item(to.last) else null
                nodes.foreach(n => {
                  toParent.insertBefore(n, before)
                })
                // might lost focus due to implementation, so we force a update!
                updateMode(None, viewUpdated = false)
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
      updateMode(update.to.mode, update.viewUpdated, fromUser = update.fromUser)
      refreshMounted()
    }))
  }

  def refreshMounted(): Unit = {
    attributeEditor.refresh()
    commandMenu.refresh()
  }

  event(window, "resize", (a: MouseEvent) => {
    refreshMounted()
  })





  var sourceEditor: SourceEditDialog = null
  var commandMenu: CommandMenuDialog = null
  var attributeEditor: UrlAttributeEditDialog = null




  private val commandMenuAnchor = new OverlayAnchor {
    override def rect: Rect = selectionRect
    override def onDismiss(): Unit = {
    }
  }

  def showCommandMenu(): Unit = {
    commandMenu.show(commandMenuAnchor)
  }


  def showAttributeEditor(cur: model.cursor.Node, pos: range.IntRange, text: model.data.Text.Delimited): Unit = {
    contentAt(cur).asInstanceOf[RichView].showAttributeEditor(pos, text)
  }




  /**
    *
    *
    * mouse event currently disabled for entire document
    *
    */


  event("mousedown", (a: MouseEvent) => {
    preventDefault(a)
  })

  event("mouseup", (a: MouseEvent) => {
    preventDefault(a)
  })


  event("click", (a: MouseEvent) => {
    preventDefault(a)
    focus() // TODO mark correct selection
  })

  event("dblclick", (a: MouseEvent) => {
    preventDefault(a)
    focus() // TODO mark correct selection
  })

  event("contextmenu", (a: MouseEvent) => {
  //  preventDefault(a)
    focus() // TODO mark correct selection
    //commandMenu.showAt(Rect(a.clientX, a.clientY, 0, 0))
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
