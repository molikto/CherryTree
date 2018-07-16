package web.view

import scalatags.JsDom.all._
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{CompositionEvent, Event, HTMLElement, HTMLImageElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, window}
import org.w3c.dom.css.CSSStyleDeclaration

import monix.execution.Scheduler.Implicits.global
import scala.scalajs.js

/**
  * it should only call methods in client for input related, lazy to build bridges now
  */
class RichView(clientView: ClientView, var rich: Rich) extends ContentView  {

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

  private def initDom(): Unit = {
    if (dom.childNodes.length == 0) {
      if (isEmpty) dom.appendChild(" ".render)
      else dom.appendChild(rec(rich.text).render)
    } else {
      throw new IllegalStateException("...")
    }
  }

  {
    initDom()
  }

  private def clearDom(): Unit = {
    insertEmptyTextNode = null
    insertNonEmptyTextNode = null
    astHighlight = null
    removeAllChild(dom)
  }

  private def clearEmptyRenderingIfEmptyState(): Unit = {
    if (isEmpty) removeAllChild(dom)
  }

  private def renderEmptyRenderingIfEmptyState(): Unit = {
    if (isEmpty && dom.childNodes.length == 0) dom.appendChild(" ".render)
  }

  private def emptyRenderingRange(): Range = {
    val range = document.createRange()
    range.setStart(dom.childNodes(0), 0)
    range.setEnd(dom.childNodes(0), 1)
    range
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

  def createTempEmptyTextNodeIn(node: Node, i: Int): (Node, Int) = {
    assert(insertEmptyTextNode == null)
    insertEmptyTextNode = document.createTextNode("")
    if (i == node.childNodes.length) {
      node.appendChild(insertEmptyTextNode)
    } else {
      node.insertBefore(insertEmptyTextNode, node.childNodes(i))
    }
    (insertEmptyTextNode, 0)
  }

  def registerNonEmptyTextNodeIn(node: Node, i: Int): (Node, Int) = {
    assert(insertNonEmptyTextNode == null)
    insertNonEmptyTextNode = node.asInstanceOf[raw.Text]
    insertNonEmptyTextNodeStartIndex = i
    insertNonEmptyTextLength = insertNonEmptyTextNode.textContent.size
    (node, i)
  }

  private def insertCursorAt(pos: Int): (Node, Int) = {
    if (pos == 0) {
      if (rich.text.head.isInstanceOf[Text.Plain]) {
        registerNonEmptyTextNodeIn(domAt(Seq(0)), 0)
      } else {
        createTempEmptyTextNodeIn(domChildArray(dom), 0)
      }
    } else if (pos == rich.size) {
      rich.text.last match {
        case plain: Text.Plain =>
          registerNonEmptyTextNodeIn(domAt(Seq(rich.text.size - 1)), plain.size)
        case _ =>
          createTempEmptyTextNodeIn(domChildArray(dom), rich.text.size)
      }
    } else {
      val ss = rich.infoSkipLeftAttributes(pos - 1)
      val ee = rich.infoSkipRightAttributes(pos)
      if (ss.ty == InfoType.Plain) {
        if (ee.ty == InfoType.Special || ee.ty == InfoType.Plain) {
          registerNonEmptyTextNodeIn(domAt(ss.nodeCursor), ss.text.asInstanceOf[Text.Plain].unicode.toStringPosition(ss.positionInUnicode + 1))
        } else {
          throw new IllegalStateException("Not possible")
        }
      } else if (ss.ty == InfoType.Special) {
        if (ee.ty == InfoType.Special) {
          if (ss.nodeCursor.size < ee.nodeCursor.size) {
            createTempEmptyTextNodeIn(domChildArray(domAt(ss.nodeCursor)), 0)
          } else {
            createTempEmptyTextNodeIn(domChildArray(domAt(ss.nodeCursor.dropRight(1))), ss.nodeCursor.last + 1)
          }
        } else if (ee.ty == InfoType.Plain) {
          registerNonEmptyTextNodeIn(domAt(ee.nodeCursor), 0)
        } else if (ee.ty == InfoType.Coded) {
          registerNonEmptyTextNodeIn(domCodeText(domAt(ee.nodeCursor)), 0)
        } else {
          throw new IllegalStateException("Not possible")
        }
      } else if (ss.ty == InfoType.Coded) {
        val unicode = ss.text.asInstanceOf[Text.Code].content
        if (ee.ty == InfoType.Special) {
          registerNonEmptyTextNodeIn(domCodeText(domAt(ee.nodeCursor)), unicode.toStringPosition(unicode.size))
        } else if (ee.ty == InfoType.Coded) {
          registerNonEmptyTextNodeIn(domCodeText(domAt(ee.nodeCursor)), unicode.toStringPosition(ee.positionInUnicode))
        } else {
          throw new IllegalStateException("Not possible")
        }
      } else {
        throw new IllegalStateException("Not possible")
      }
    }
  }

  private def nonEmptySelectionToDomRange(range: IntRange): (Node, Int, Node, Int, HTMLSpanElement) = {
    // there are three cases of a selection
    // a subparagraph, a sub-code, a delimiter of format/code node
    val ss = rich.info(range.start)
    val ee = rich.info(range.until - 1)
    if (ss.ty == InfoType.Coded &&
      ee.ty == InfoType.Coded &&
      ss.nodeCursor == ee.nodeCursor &&
      ss.text.isInstanceOf[Text.Code]) {
      val codeText = domCodeText(domAt(ss.nodeCursor))
      val ast = ss.text.asInstanceOf[Text.Code]
      val sss = ast.content.toStringPosition(ss.positionInUnicode)
      val eee = ast.content.toStringPosition(ee.positionInUnicode + 1)
      (codeText, sss, codeText, eee, null)
    } else if (range.size == 1 &&
      ss.ty  == InfoType.Special &&
      SpecialChar.startsEnds.contains(ss.specialChar) &&
      !ss.text.isInstanceOf[Text.AtomicSelected]) {
      val isStart = SpecialChar.starts.contains(ss.specialChar)
      val a = domAt(ss.nodeCursor).asInstanceOf[HTMLSpanElement]
      val range = if (isStart) (0, 1) else (2, 3)
      (a, range._1, a, range._2, a)
    } else {
      val start = if (ss.ty == InfoType.Plain) {
        val text = domAt(ss.nodeCursor)
        val s = ss.text.asInstanceOf[Text.Plain].unicode.toStringPosition(ss.positionInUnicode)
        (text, s)
      } else {
        assert(ss.isStart)
        val node = domChildArray(domAt(ss.nodeCursor.dropRight(1)))
        (node, ss.nodeCursor.last)
      }
      val end = if (ee.ty == InfoType.Plain) {
        val text = domAt(ee.nodeCursor)
        val e = ee.text.asInstanceOf[Text.Plain].unicode.toStringPosition(ss.positionInUnicode + 1)
        (text, e)
      } else {
        assert(ee.isEnd)
        val node = domChildArray(domAt(ee.nodeCursor.dropRight(1)))
        (node, ee.nodeCursor.last + 1)
      }
      (start._1, start._2, end._1, end._2, null)
    }
  }


  private def removeFormattedNodeHighlight(): Unit = {
    if (astHighlight != null) {
      astHighlight.style.backgroundColor = null
      astHighlight = null
    }
  }

  private def addFormattedNodeHighlight(_5: HTMLSpanElement): Unit = {
    astHighlight = _5
    _5.style.backgroundColor = clientView.theme.astHighlight
  }

  /**
    *
    *
    * events
    *
    */


  event("compositionstart", (a: CompositionEvent) => {
    if (isInserting) clientView.client.disableStateUpdate = true
    else a.preventDefault()
  })

  event("compositionupdate", (a: CompositionEvent) => {
    if (!isInserting) a.preventDefault()
  })

  event("compositionend", (a: CompositionEvent) => {
    if (isInserting) clientView.client.disableStateUpdate = false
    else a.preventDefault()
  })

  event("input", (a: Event) => {
    // TODO only accept single node text changes, or subparagraph changes??
    if (isInserting) {
      if (a.asInstanceOf[js.Dynamic].inputType.asInstanceOf[String] == "insertText"
      ) {
        // should be pick up by our keyboard handling
        clientView.client.flush()
      } else {
        window.console.log(a)
      }
    } else {
      a.preventDefault()
    }
  })


  /**
    *
    * mode rendering
    *
    * in mode rendering we always assume the content is rendered correctly
    *
    *
    */

  def flushInsertionMode(): Unit = {
    if (insertEmptyTextNode != null) {
      // this is really ugly, but somehow Chrome create a new TextNode???
      var previous = insertEmptyTextNode.previousSibling
      var next = insertEmptyTextNode.nextSibling
      var str = ""
      if (previous != null && previous.isInstanceOf[raw.Text]) {
        str = str + previous.textContent
      } else {
        previous = null
      }
      if (insertEmptyTextNode.textContent.length > 0) {
        str = if (str.isEmpty) insertEmptyTextNode.textContent else str + insertEmptyTextNode.textContent
      }
      if (next.textContent.length > 0 && next.isInstanceOf[raw.Text]) {
        str = str + next.textContent
      } else {
        next = null
      }
      if (previous != null) previous.parentNode.removeChild(previous)
      if (next != null) next.parentNode.removeChild(next)
      if (str.length > 0) {
        insertEmptyTextNode.textContent = str
        clientView.client.onInsertRichTextAndViewUpdated(Unicode(str))
        insertNonEmptyTextNode = insertEmptyTextNode
        insertNonEmptyTextLength = str.length
        insertNonEmptyTextNodeStartIndex = insertNonEmptyTextLength
        insertEmptyTextNode = null
      }
    } else if (insertNonEmptyTextNode != null) {
      val newContent = insertNonEmptyTextNode.textContent
      val insertion = newContent.substring(
        insertNonEmptyTextNodeStartIndex, insertNonEmptyTextNodeStartIndex + newContent.length - insertNonEmptyTextLength)
      if (insertion.length > 0) {
        clientView.client.onInsertRichTextAndViewUpdated(Unicode(insertion))
        insertNonEmptyTextLength = newContent.length
        insertNonEmptyTextNodeStartIndex += insertion.length
      }
    }
  }



  private def clearInsertionMode(): Unit = {
    if (insertEmptyTextNode != null) {
      insertEmptyTextNode.parentNode.removeChild(insertEmptyTextNode)
      insertEmptyTextNode = null
    }
    insertNonEmptyTextNode = null
    if (flushSubscription != null) {
      flushSubscription.cancel()
      flushSubscription = null
    }
  }

  private def clearVisualMode(): Unit = {
    // TODO visual mode
  }
  private def updateVisualMode(fix: IntRange, move: IntRange): Unit = {
    // TODO visual mode
  }

  private def clearNormalMode(): Unit = {
    val sel = window.getSelection
    if (sel.rangeCount > 0) sel.removeAllRanges
    removeFormattedNodeHighlight()
  }

  private def updateInsertMode(pos: Int): Unit = {
    if (flushSubscription == null) {
      flushSubscription = defer(clientView.client.flushes.doOnNext(_ => {
        flushInsertionMode()
      }).subscribe())
    }
    val range = document.createRange()
    if (isEmpty) {
      clearEmptyRenderingIfEmptyState()
      insertEmptyTextNode = document.createTextNode("")
      dom.appendChild(insertEmptyTextNode)
    }  else {
      val start = insertCursorAt(pos)
      range.setStart(start._1, start._2)
      range.setEnd(start._1, start._2)
    }
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
  }


  private def updateNormalMode(r: IntRange): Unit = {
    val range = if (isEmpty) {
      emptyRenderingRange()
    } else {
      val range = document.createRange()
      if (r.isEmpty) {
        throw new IllegalStateException("Normal mode should not have empty range if rich is not empty")
      } else {
        val start = nonEmptySelectionToDomRange(r)
        range.setStart(start._1, start._2)
        range.setEnd(start._3, start._4)
        if (start._5 != astHighlight) removeFormattedNodeHighlight()
        if (start._5 != null) addFormattedNodeHighlight(start._5)
      }
      range
    }
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
  }

  override def clearMode(): Unit = {
    dom.contentEditable = "false"
  }

  override def initMode(): Unit = {
    dom.contentEditable = "true"
  }

  private def isInserting = flushSubscription != null
  private var flushSubscription: Cancelable = null

  override def updateMode(aa: mode.Content, viewUpdated: Boolean): Unit = {
    if (viewUpdated) return
    aa match {
      case mode.Content.Insertion(pos) =>
        clearNormalMode()
        clearVisualMode()
        clearEmptyRenderingIfEmptyState()
        updateInsertMode(pos)
      case mode.Content.Visual(fix, move) =>
        clearInsertionMode()
        clearNormalMode()
        renderEmptyRenderingIfEmptyState()
        updateVisualMode(fix, move)
      case mode.Content.Normal(range) =>
        clearInsertionMode()
        clearVisualMode()
        renderEmptyRenderingIfEmptyState()
        updateNormalMode(range)
    }
  }

  override def updateContent(data: model.data.Content, c: operation.Content, viewUpdated: Boolean): Unit = {
    rich = data.asInstanceOf[model.data.Content.Rich].content
    isEmpty = rich.isEmpty
    if (!viewUpdated) {
      // TODO incrementally update dom
     // val cs = c.asInstanceOf[operation.Content.Rich]
    }
    clearDom()
    initDom()
  }
}
