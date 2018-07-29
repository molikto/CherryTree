package web.view.doc

import command.Key
import doc.{DocInterface, DocState}
import model.data.Content
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{html, window, document}
import scalatags.JsDom.all._
import view.EditorInterface
import web.view.content.{CodeView, ContentView, RichView}
import web.view.{KeyMap, View, theme}

import scala.collection.mutable.ArrayBuffer

class DocumentView(private val client: DocInterface, override protected val editor: EditorInterface) extends EditorView {

  dom = div(
    width := "100%",
    height := "100%",
    padding := "48px",
    color := theme.contentText,
    `class` := "ct-root ct-scroll",
    overflowY := "scroll"
  ).render

  /**
    *
    *
    *
    * state
    *
    *
    */
  private val noEditable = div(contenteditable := true, width := "0px", height := "0px").render
  private var currentEditable: HTMLElement = noEditable
  private var isFocusedOut: Boolean = false
  private var duringStateUpdate: Boolean = false

  event("focusout", (a: FocusEvent) => {
    a.relatedTarget match {
      case h: HTMLElement if dom.contains(h) =>
      case _ =>
        if (!model.debugDisableFocusHandling) {
          if (!duringStateUpdate) {
            isFocusedOut = true
            updateMode(None, viewUpdated = false)
          }
        }
    }
  })

  event("focusin", (a: FocusEvent) => {
    if (!model.debugDisableFocusHandling) {
      if (!duringStateUpdate && isFocusedOut) {
        isFocusedOut = false
        updateMode(client.state.mode, viewUpdated = false)
      }
    }
  })

  override def markEditable(dom: HTMLElement): Unit = {
    if (currentEditable == dom) return
    if (currentEditable != noEditable) {
      throw new IllegalArgumentException("You shouldn't mark editable if other is also editable")
    }
    dom.contentEditable = "true"
    noEditable.contentEditable = "false"
    currentEditable = dom
    currentEditable.focus()
  }

  override def unmarkEditableIfEditable(dom: HTMLElement): Unit = {
    if (dom == currentEditable) unmarkEditable(dom)
  }

  override def unmarkEditable(dom: HTMLElement): Unit = {
    if (currentEditable == noEditable) return
    if (currentEditable != dom) {
      throw new IllegalArgumentException("You shouldn't unmark editable if you are not the one")
    }
    dom.contentEditable = "false"
    currentEditable = noEditable
    noEditable.contentEditable = "true"
    noEditable.focus()
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
  private def childListAt(at: model.cursor.Node): HTMLElement = {
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a
      // ul, li, content
      else rec(a.childNodes(b.head).childNodes(0).childNodes(1), b.tail)
    }
    rec(dom.childNodes(0).childNodes(1), at).asInstanceOf[HTMLElement]
  }

  private def contentAt(at: model.cursor.Node): ContentView.General = {
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a.childNodes(0)
      // ul, li, content
      else rec(a.childNodes(1).childNodes(b.head).childNodes(0), b.tail)
    }
    View.fromDom[ContentView.General](rec(dom.childNodes(0), at))
  }

  private var previousNodeVisual: ArrayBuffer[Element] = new ArrayBuffer[Element]()

  private def updateNodeVisual(v: model.mode.Node.Visual): Unit = {
    val newVisual = new ArrayBuffer[Element]()
    def update(overall: HTMLElement, start: Int, u: Int): Unit = {
      for (i <- start until u) {
        val c = overall.children(i).children(0)
        newVisual.append(c)
      }
    }
    val overall = model.cursor.Node.minimalRange(v.fix, v.move)
    overall match {
      case None =>
        val rd = dom.children(0)
        newVisual.append(rd)
      case Some(range) => update(childListAt(range.parent), range.childs.start, range.childs.until)
    }
    (newVisual -- previousNodeVisual).foreach(_.classList.add("ct-node-visual"))
    (previousNodeVisual -- newVisual).foreach(_.classList.remove("ct-node-visual"))
    previousNodeVisual = newVisual
  }

  private def clearNodeVisual(): Unit = {
    for (c <- previousNodeVisual) {
      c.classList.remove("ct-node-visual")
    }
    previousNodeVisual = ArrayBuffer.empty
  }



  private def removeNodes(range: model.range.Node): Unit = {
    def destroyContents(a: Element, start: Int, u: Int): Unit = {
      for (i <- start until u) {
        val ll = a.children(i).children(0).children(1)
        destroyContents(ll, 0, ll.children.length)
        View.fromDom[ContentView.General](a.children(i).children(0).children(0)).destroy()
      }
    }
    val p = childListAt(range.parent)
    destroyContents(p, range.childs.start, range.childs.until)
    for (_ <- range.childs) {
      p.removeChild(p.children(range.childs.start))
    }
  }

  private def insertNodes(list: HTMLElement, at: Int, contents: Seq[model.data.Node]): Unit = {
    val before = if (at == list.childNodes.length) null else  list.childNodes.apply(at)
    contents.foreach(a => {
      val item = div(paddingLeft := "24px").render
      list.insertBefore(item, before)
      insertNodesRec(a, item)
    })
  }

  private def insertNodesRec(root: model.data.Node, parent: html.Element): Unit = {
    val box = div().render
    parent.insertBefore(box, parent.firstChild)
    createContent(root.content).attachToNode(box)
    val list = div().render
    box.appendChild(list)
    insertNodes(list, 0, root.childs)
  }


  private def createContent(c: Content): ContentView.General = {
    c match {
      case model.data.Content.Rich(cs) =>
        new RichView(this, editor, cs).asInstanceOf[ContentView.General]
      case c@model.data.Content.Code(_, _) =>
        new CodeView(this, editor, c).asInstanceOf[ContentView.General]
    }
  }

  private def updateMode(mm: Option[model.mode.Node], viewUpdated: Boolean): Unit = {
    duringStateUpdate = true
    val m = if (isFocusedOut) None else mm
    m match {
      case None =>
        removeFocusContent()
        clearNodeVisual()
      case Some(mm) => mm match {
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
              current.asInstanceOf[RichView].updateMode(r, viewUpdated)
            case c: model.mode.Content.Code =>
              current.asInstanceOf[CodeView].updateMode(c, viewUpdated)
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
    val DocState(node, selection) = client.state
    insertNodesRec(node, dom)
    dom.appendChild(noEditable) // dom contains this random thing, this is ok now, but... damn
    updateMode(selection, viewUpdated = false)

    observe(client.stateUpdates.doOnNext(update => {
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
      updateMode(update.mode, update.viewUpdated)
    }))

  }








  /**
    *
    *
    * mouse event currently disabled for entire document
    *
    */


  event("mousedown", (a: MouseEvent) => {
    if (!isFocusedOut) preventDefault(a)
    else window.console.log(a)
  })

  event("mouseup", (a: MouseEvent) => {
    if (!isFocusedOut) preventDefault(a)
    else window.console.log(a)
  })


  event("click", (a: MouseEvent) => {
    if (!isFocusedOut) preventDefault(a)
    else window.console.log(a)
  })

  event("dblclick", (a: MouseEvent) => {
    if (!isFocusedOut) preventDefault(a)
    else window.console.log(a)
  })

  event("contextmenu", (a: MouseEvent) => {
    window.console.log(a)
    // LATER fix this??
    preventDefault(a)
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
