package web.ui.doc

import api.PermissionLevel
import command.Key
import doc.DocState.NodeAndType
import doc.{DocInterface, DocState}
import model.data.NodeType
import model.{cursor, data, range}
import model.data.{Node => _, _}
import model.mode.Content.RichInsert
import model.range.IntRange
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw._
import org.scalajs.dom.{html, window}
import org.scalajs.dom.{document, html, window}
import scalatags.JsDom.all._
import settings.Settings
import util.Rect
import view.EditorInterface
import web.ui
import web.ui.content._
import web.view.{OverlayAnchor, _}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class SimpleLayoutDocumentView(
  override val settings: Settings,
  override protected val client: DocInterface,
  override protected val editor: EditorInterface,
  override val latexMacroCache: LaTeXMacroCache
) extends DocumentView with EditorView {


  private val rootFrame = div(
    width := "100%"
  ).render

  dom.appendChild(div(cls := "unselectable", height := "36px",  display := "block", contenteditable := "false").render)
  dom.appendChild(rootFrame)
  dom.appendChild(div(cls := "unselectable", height := "246px",  display := "block", contenteditable := "false").render)

  /**
    *
    * node list
    *
    *
    */
  //   frame = rootframe
  //     box
  //       hold
  //       content
  //     child-list
  //       frame ...
  //    nonEditable...

  private def frameAt(at: model.cursor.Node): HTMLElement = {
    if (rootFrame == this.rootFrame) assert(currentDoc == null || currentDoc.inViewport(at), s"not in viewport $at, current view port is $currentZoom")
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a
      else rec(a.childNodes(1).childNodes(b.head), b.tail)
    }
    rec(rootFrame, at.drop(currentZoom.size)).asInstanceOf[HTMLElement]
  }

  private def childListAt(at: model.cursor.Node): HTMLElement = {
    frameAt(at).childNodes(1).asInstanceOf[HTMLElement]
  }

  private def childListOf(frame: HTMLElement): HTMLElement = {
    frame.childNodes(1).asInstanceOf[HTMLElement]
  }

  private def frameInList(parent: HTMLElement, at: Int) = parent.childNodes(at).asInstanceOf[HTMLElement]


  private def holdAt(at: model.cursor.Node): HTMLElement = {
    frameAt(at).childNodes(0).childNodes(0).asInstanceOf[HTMLElement]
  }

  private def holdOf(at: HTMLElement): HTMLElement = {
    at.childNodes(0).childNodes(0).asInstanceOf[HTMLElement]
  }

  override protected def contentOfHold(a: Node): ContentView.General = View.fromDom(a.nextSibling)

  private def contentOf(frame: HTMLElement): ContentView.General = {
    val v = frame.childNodes(0).childNodes(1).asInstanceOf[HTMLElement]
    View.fromDom[ContentView.General](v)
  }

  override protected def contentAt(at: model.cursor.Node): ContentView.General = {
    val v = frameAt(at).childNodes(0).childNodes(1).asInstanceOf[HTMLElement]
    View.fromDom[ContentView.General](v)
  }

  override protected  def cursorOf[T <: model.data.Content, O <: model.operation.Content](a: ContentView[T, O]): model.cursor.Node = {
    def rec(frame: Node): Seq[Int] = {
      if (frame == rootFrame) {
        currentZoom
      } else {
        val list = frame.parentNode
        val parent = list.childNodes
        var i = -1
        var j = 0
        while (i < 0 && j < parent.length) {
          if (frame == parent(j)) {
            i = j
          }
          j += 1
        }
        rec(list.parentNode) :+ i
      }
    }
    rec(a.dom.parentNode.parentNode)
  }

  private var previousNodeVisual: ArrayBuffer[Element] = new ArrayBuffer[Element]()
  private var previousNodeMove: HTMLElement = null


  override def updateNodeVisual(v: model.mode.Node.Visual, fromUser: Boolean): Unit = {
    val newVisual = new ArrayBuffer[Element]()
    val overall = model.cursor.Node.minimalRange(v.fix, v.move)
    overall match {
      case None =>
        val rd = frameAt(currentZoom)
        newVisual.append(rd)
      case Some(range) => range.foreach(c => newVisual.append(frameAt(c)))
    }
    (newVisual -- previousNodeVisual).foreach(_.classList.add("ct-node-visual"))
    (previousNodeVisual -- newVisual).foreach(_.classList.remove("ct-node-visual"))
    previousNodeVisual = newVisual
    val newMove = frameAt(v.move)
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

  override def clearNodeVisual(): Unit = {
    if (previousNodeMove != null) {
      for (c <- previousNodeVisual) {
        c.classList.remove("ct-node-visual")
      }
      previousNodeVisual = ArrayBuffer.empty
      if (previousNodeMove != null) previousNodeMove.classList.remove("ct-node-visual-move")
      previousNodeMove = null
    }
  }



  override def removeAllNodes(): Unit = {
    val a = rootFrame
    removeNodes(model.range.Node(currentZoom, IntRange(0, childListAt(currentZoom).childNodes.length)))
    contentAt(currentZoom).destroy()
    a.removeChild(a.childNodes(0))
    a.removeChild(a.childNodes(0))
  }

  private def destroyContents(a: HTMLElement, start: Int, u: Int): Unit = {
    for (i <- start until u) {
      val frame = frameInList(a, i)
      val ll = childListOf(frame)
      destroyContents(ll, 0, ll.children.length)
      contentOf(frame).destroy()
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



  private def toggleHoldRendering(doc: DocState, cur: model.cursor.Node, node: model.data.Node, frame0: Node, childlist: Node, fold: Boolean): Unit = {
    val frame = frame0.asInstanceOf[HTMLElement]
    val cl = if (childlist == null) null else childlist.asInstanceOf[HTMLElement]
    if (fold) {
      if (!frame.classList.contains("ct-folded")) {
        frame.classList.add("ct-folded")
        if (cl != null) {
          destroyContents(cl, 0, cl.childNodes.length)
          removeAllChild(cl)
        }
      }
    } else {
      if (frame.classList.contains("ct-folded")) {
        frame.classList.remove("ct-folded")
        if (cl != null) insertNodes(doc, cur, cl, 0, node.childs)
      }
    }
  }



  private def insertNodeRec(folderType: model.data.NodeType, cur: model.cursor.Node, root: model.data.Node, rootType: model.data.NodeType, parent: html.Element): Unit = {
    val firstChild = parent.firstChild
    val box = div(cls := "ct-box").render
    parent.insertBefore(box, firstChild)
    val hold = tag("div")(contenteditable := "false", cls := "ct-hold").render
    hold.addEventListener("mouseover", handleHoverEvent)
    box.appendChild(hold)
    contentViewCreate(root.content, canEditNode(root)).attachToNode(box)
    val list = div(cls := "ct-d-childlist").render
    parent.insertBefore(list, firstChild)
    val folded = currentDoc.viewAsFolded(root)
    if (folded) {
     parent.classList.add("ct-folded")
    } else {
      insertNodes(folderType, cur, rootType, list, 0, root.childs)
    }
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


  def refreshAllLaTeX(): Unit = {
    if (model.debug_katex) window.console.log("refreshing all latex")
    def rec(frame: HTMLElement): Unit = {
      contentOf(frame).refreshLaTeX()
      var c = childListOf(frame).firstChild
      while (c != null) {
        rec(c.asInstanceOf[HTMLElement])
        c = c.nextSibling
      }
    }
    rec(rootFrame)
  }

  def renderAll(): Unit = {
    val NodeAndType(node, folder, nt) = currentDoc.nodeAndType(currentZoom)
    rootFrame.className = classesFromNodeAttribute(folder, node, nt)
    insertNodeRec(folder, currentZoom, node, nt, rootFrame)
  }

  private def insertNodes(
                         folderType: NodeType,
                         parentCur: model.cursor.Node,
                         parentType: NodeType,
                          list: HTMLElement,
                          at: Int, contents: Seq[model.data.Node]
                         ): Unit = {
    val before = if (at == list.childNodes.length) null else  list.childNodes.apply(at)
    contents.zipWithIndex.foreach(a => {
      val nF = a._1.attribute(NodeType).filter(_.isFolder).getOrElse(folderType)
      val pt = a._1.nodeType(nF, parentType)
      val frame = div(cls := classesFromNodeAttribute(nF, a._1, pt)).render
      list.insertBefore(frame, before)
      insertNodeRec(nF, parentCur :+ a._2, a._1, pt, frame)
    })
  }

  private def insertNodes(doc: DocState, parentCur: model.cursor.Node, list: HTMLElement, at: Int, contents: Seq[model.data.Node]): Unit = {
    val NodeAndType(node, folderType, parentType) =  doc.nodeAndType(parentCur)
    insertNodes(folderType, parentCur, parentType, list, at, contents)
  }


  def toggleHold(a: model.cursor.Node, visible: Boolean): Unit = {
    val fr = frameAt(a)
    toggleHoldRendering(currentDoc, a, currentDoc.node(a), fr, childListOf(fr), visible)
  }

  def syncNodeType(list: HTMLElement, from: Int, ns: Seq[model.data.Node], folder: NodeType, t: NodeType): Unit = {
    if (list.childNodes.length > 0) {
      for (i <- from until (from + ns.size)) {
        val n = ns(i - from)
        val frame = frameInList(list, i)
        val nt = n.nodeType(folder, t)
        modifyClassNameExceptFold(frame, classesFromNodeAttribute(folder, n, nt))
        val cl = childListOf(frame)
        if (nt.isFolder) {
          // no need to sync anymore, because it is already correct
        } else {
          syncNodeType(cl, 0, n.childs, folder, nt)
        }
      }
    }
  }

  def modifyClassNameExceptFold(frame: HTMLElement, str: String) = {
    if (frame.classList.contains("ct-folded")) {
      frame.className = str
      frame.classList.add("ct-folded")
    } else {
      frame.className = str
    }
  }

  def renderTransaction(s: DocState, t: model.operation.Node, to: DocState, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    def replaceContent(at: model.cursor.Node, c: model.data.Node) = {
      val previousContent = contentAt(at)
      val p = previousContent.dom.parentNode
      val before = previousContent.dom.nextSibling
      previousContent.destroy()
      contentViewCreate(c.content, canEditNode(c)).attachToNode(p, before.asInstanceOf[HTMLElement])
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
            activeContentEditor.updateContent(to.node(at).content, m, c, viewUpdated, editorUpdated)
          } else {
            content.updateContent(to.node(at).content, c, viewUpdated)
          }
        }
      case model.operation.Node.AttributeChange(at, _, _) =>
        if (s.visible(at)) {
          val old = to.node(at)
          if (!contentViewMatches(old.content, contentAt(at))) {
            replaceContent(at, old)
          }
          val NodeAndType(node, folder, t) = to.nodeAndType(at)
          val frame = frameAt(at)
          modifyClassNameExceptFold(frame, classesFromNodeAttribute(folder, node, t))
          toggleHoldRendering(s, at, old, frame, childListOf(frame), to.viewAsFolded(at))
          syncNodeType(childListOf(frame), 0, node.childs, folder, t)
          clearNodeVisual() // previous is not valid maybe
        }
      case model.operation.Node.Replace(at, c) =>
        if (s.visible(at)) {
          val node = to.node(at)
          replaceContent(at, node)
        }
      case model.operation.Node.Delete(r) =>
        if (s.viewAsNotFoldedAndNotHidden(r.parent)) {
          removeNodes(r)
        }
      case model.operation.Node.Insert(at, childs) =>
        val pCur = model.cursor.Node.parent(at)
        if (s.visible(at)) {
          val root = childListAt(pCur)
          insertNodes(s, pCur, root, at.last, childs)
        }
      case model.operation.Node.Move(range, to) =>
        val toP = model.cursor.Node.parent(to)
        if (s.viewAsNotFoldedAndNotHidden(range.parent) && s.visible(to)) {
          val NodeAndType(_, folder, pt) = s.nodeAndType(toP)
          val parent = childListAt(range.parent)
          val toParent = childListAt(toP)
          val nodes = range.childs.map(i => parent.childNodes.item(i)).toSeq
          val before = if (to.last < toParent.childNodes.length)  toParent.childNodes.item(to.last) else null
          nodes.foreach(n => {
            toParent.insertBefore(n, before)
          })
          syncNodeType(toParent, to.last, s.node(range), folder, pt)
        } else if (s.viewAsNotFoldedAndNotHidden(range.parent)) {
          removeNodes(range)
        } else if (s.visible(to)) {
          val data = s.node(range)
          val p = model.cursor.Node.parent(to)
          val root = childListAt(p)
          insertNodes(s, p, root, to.last, data)
        }
    }
  }


  def scrollToTop(cur: model.cursor.Node): Unit = {
    contentAt(cur).dom.scrollIntoView(true)
    dom.scrollTop  = Math.max(0, dom.scrollTop - 10)
  }

}
