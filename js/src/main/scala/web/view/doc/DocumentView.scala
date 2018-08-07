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

  private def frameAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a
      else rec(a.childNodes(1).childNodes(1).childNodes(b.head), b.tail)
    }
    rec(rootFrame, at).asInstanceOf[HTMLElement]
  }

  private def boxAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    frameAt(at, rootFrame).childNodes(1).asInstanceOf[HTMLElement]
  }

  private def childListAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    boxAt(at, rootFrame).childNodes(1).asInstanceOf[HTMLElement]
  }

  private def frameInList(parent: HTMLElement, at: Int) = parent.childNodes(at).asInstanceOf[HTMLElement]


  private def holdAt(at: model.cursor.Node, rootFrame: Node = rootFrame): HTMLElement = {
    frameAt(at, rootFrame).childNodes(0).asInstanceOf[HTMLElement]
  }

  private def contentAt(at: model.cursor.Node, rootFrame: Node = rootFrame): ContentView.General = {
    val v = boxAt(at, rootFrame).childNodes(0).asInstanceOf[HTMLElement]
    View.fromDom[ContentView.General](v)
  }

  def cursorOf[T <: model.data.Content, O <: model.operation.Content, M <: model.mode.Content](a: ContentView[T, O, M]): model.cursor.Node = {
    def rec(a: Node): Seq[Int] = {
      val frame = a.parentNode.parentNode
      if (frame == rootFrame) {
        Seq.empty
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
        val rd = boxAt(model.cursor.Node.root)
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
      if (v.move == model.cursor.Node.root) {
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

  private def insertNodes(list: HTMLElement, at: Int, contents: Seq[model.data.Node]): Unit = {
    val before = if (at == list.childNodes.length) null else  list.childNodes.apply(at)
    contents.foreach(a => {
      val frame = div(`class` := "ct-d-frame").render
      list.insertBefore(frame, before)
      insertNodesRec(a, frame)
    })
  }


  private val viewBoxStr = {
    val padding = 128
    val size = 512 + padding * 2
    s"-$padding -$padding $size $size"
  }
  private val holdBg = svgSourceToBackgroundStr(
    s"""
       |<svg xmlns="http://www.w3.org/2000/svg" viewBox="$viewBoxStr"><path style="fill: #555c75;" d="M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8z"/></svg>
     """.stripMargin)

  private val holdFoldBg = svgSourceToBackgroundStr(
    s"""
       |<svg xmlns="http://www.w3.org/2000/svg" viewBox="$viewBoxStr"><path style="fill: #555c75;" d="M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm144 276c0 6.6-5.4 12-12 12h-92v92c0 6.6-5.4 12-12 12h-56c-6.6 0-12-5.4-12-12v-92h-92c-6.6 0-12-5.4-12-12v-56c0-6.6 5.4-12 12-12h92v-92c0-6.6 5.4-12 12-12h56c6.6 0 12 5.4 12 12v92h92c6.6 0 12 5.4 12 12v56z"/></svg>
     """.stripMargin)

  private def toggleHoldRendering(hold0: Node, fold: Boolean): Unit = {
    val hold = hold0.asInstanceOf[HTMLElement]
    if (fold) {
      hold.style.background = holdFoldBg
    } else {
      hold.style.background = holdBg
    }
  }
  private def insertNodesRec(root: model.data.Node, parent: html.Element): Unit = {
    val firstChild = parent.firstChild
    val hold = tag("i")(`class` := "ct-d-hold").render
    parent.insertBefore(hold, firstChild)
    val box = div(`class` := "ct-d-box").render
    parent.insertBefore(box, firstChild)
    createContent(root.content).attachToNode(box)
    val list = div().render
    // LATER mmm... this is a wired thing. can it be done more efficiently, like not creating the list at all?
    if (client.state.folded(root)) {
      list.classList.add("ct-folded")
      toggleHoldRendering(hold, true)
    } else {
      toggleHoldRendering(hold, false)
    }
    box.appendChild(list)
    insertNodes(list, 0, root.childs)
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
    val DocState(node, selection, _) = client.state
    insertNodesRec(node, rootFrame)
    updateMode(selection, viewUpdated = false)

    observe(client.stateUpdates.doOnNext(update => {
      update.folds.foreach(f => {
        toggleHoldRendering(holdAt(f._1), f._2)
        if (f._2) {
          childListAt(f._1).classList.add("ct-folded")
        } else {
          childListAt(f._1).classList.remove("ct-folded")
        }
      })
      duringStateUpdate = true
      for (t <- update.transaction) {
        t match {
          case model.operation.Node.Content(at, c) =>
            contentAt(at).updateContent(update.root(at).content, c, update.viewUpdated)
          case model.operation.Node.Replace(at, c) =>
            val previousContent = contentAt(at)
            val p = previousContent.dom.parentNode
            createContent(c).attachToNode(p, previousContent.dom)
            previousContent.destroy()
          case model.operation.Node.Delete(r) =>
            // look out for this!!!
            removeNodes(r)
          case model.operation.Node.Insert(at, childs) =>
            val root = childListAt(at.dropRight(1))
            insertNodes(root, at.last, childs)
          case model.operation.Node.Move(range, to) =>
            val parent = childListAt(range.parent)
            val toParent = childListAt(to.dropRight(1))
            val nodes = range.childs.map(i => parent.childNodes.item(i)).toSeq
            val before = if (to.last < toParent.childNodes.length)  toParent.childNodes.item(to.last) else null
            nodes.foreach(n => {
              toParent.insertBefore(n, before)
            })
            // might lost focus due to implementation, so we force a update!
            updateMode(None, viewUpdated = false)
        }
      }
      duringStateUpdate = false
      updateMode(update.mode, update.viewUpdated, fromUser = update.fromUser)
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
