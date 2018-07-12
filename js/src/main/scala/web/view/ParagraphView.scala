package web.view

import scalatags.JsDom.all._
import model._
import model.data.{InfoType, Paragraph, SpecialChar, Text}
import model.range.IntRange
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLImageElement, HTMLSpanElement}
import org.w3c.dom.css.CSSStyleDeclaration

import scala.scalajs.js

class ParagraphView(clientView: ClientView, var paragraph: Paragraph) extends ContentView  {

  private val inserting: ParagraphEditorView = null

  def rec(seq: Seq[model.data.Text]): Seq[Frag] = {
    seq.map {
      case Text.Emphasis(c) => span(
        span(`class` := "ct-cg", "*"),
        em(`class` := "ct-em", rec(c)),
        span(`class` := "ct-cg", "*")
      )
      case Text.Strong(c) => span(
        backgroundColor := clientView.theme.astHighlight,
        span(`class` := "ct-cg", "#"),
        strong(`class` := "ct-em", rec(c)),
        span(`class` := "ct-cg", "#") // TODO ** char as a single char
      )
      case Text.StrikeThrough(c) => span(
        span(`class` := "ct-cg", "-"),
        del(`class` := "ct-del", rec(c)),
        span(`class` := "ct-cg", "-")
      )
      case Text.Link(t, b, c) => span(
        span(`class` := "ct-cg", "["),
        span(`class` := "ct-link", rec(t), href := b.toString),
        span(`class` := "ct-cg", "]")
      )
      case Text.Image(t, b, c) =>
        img(rec(t), verticalAlign := "bottom", src := b.toString)
      case Text.LaTeX(c) =>
        val a = span(`class` := "ct-latex").render
        window.asInstanceOf[js.Dynamic].katex.render(c.toString, a)
        bindNode(a)
      case Text.Code(c) =>
        span(
          span(`class` := "ct-cg", "`"),
          code(`class` := "ct-code", c.toString),
          span(`class` := "ct-cg", "`")
        )
      case Text.Plain(c) => stringFrag(c.toString)
    }
  }

  var isEmpty = paragraph.isEmpty

  dom = p(`class` := "ct-content", if (isEmpty) " " else rec(paragraph.text)).render

  dom.style = "outline: 0px solid transparent;"

  event("compositionstart", (a: CompositionEvent) => {
    if (inserting == null) a.preventDefault()
    else inserting.onCompositionStart(a)
  })

  event("compositionupdate", (a: CompositionEvent) => {
    if (inserting == null) a.preventDefault()
    else inserting.onCompositionUpdate(a)
  })

  event("compositionend", (a: CompositionEvent) => {
    if (inserting == null) a.preventDefault()
    else inserting.onCompositionUpdate(a)
  })

  event("input", (a: Event) => {
    if (inserting == null) a.preventDefault()
    else inserting.onInput(a)
  })

  private val inserter: ParagraphEditorView = null


  def removeInsertionModeIfExists(): Unit = {
    // TODO
  }


  def getDom(a: Seq[Int]): Node = getDom(dom, a)

  def getDom(parent: Node, a: Seq[Int]): Node = {
    if (a.isEmpty) {
      parent
    } else {
      if (parent.isInstanceOf[HTMLImageElement]) {
        getDom(parent.childNodes.item(a.head), a.tail)
      } else {
        getDom(parent.childNodes.item(1).childNodes.item(a.head), a.tail)
      }
    }
  }

  def selectionToDomRange(range: IntRange): (Node, Int, Int) = {
    // there are three cases of a selection
    // a subparagraph, a sub-code, a delimiter of format/code node
    val info = paragraph.info
    val ss = info(range.start)
    val ee = info(range.endInclusive)
    if (ss.ty == InfoType.Coded &&
      ee.ty == InfoType.Coded &&
      ss.nodePosition == ee.nodePosition &&
      ss.text.isInstanceOf[Text.Code]) {
      val codeText = getDom(ss.nodePosition).childNodes.item(1)
      val ast = ss.text.asInstanceOf[Text.Code]
      val sss = ast.unicode.toStringPosition(ss.charPosition)
      val eee = ast.unicode.toStringPosition(ee.charPosition + 1)
      (codeText, sss, eee)
    } else if (range.size == 1 &&
      ss.ty  == InfoType.Special &&
      SpecialChar.startsEnds.contains(ss.specialChar) &&
      !ss.text.isInstanceOf[Text.AtomicViewed]) {
      val isStart = SpecialChar.starts.contains(ss.specialChar)
      val a = getDom(ss.nodePosition)
      val range = if (isStart) (0, 1) else (2, 3)
      (a, range._1, range._2)
    } else {
      (dom, 0, dom.childNodes.length) // TODO
    }
  }

  def setSelection(r: IntRange): Unit = {
    val range: Range = document.createRange()
    if (isEmpty) {
      range.setStart(dom.childNodes.item(0), 0)
      range.setEnd(dom.childNodes.item(0), 1)
    } else {
      val start = selectionToDomRange(r)
      range.setStart(start._1, start._2)
      range.setEnd(start._1, start._3)
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

  override def syncMode(aa: mode.Content): Unit =  aa match {
    case mode.Content.Insertion(pos) =>
      // TODO remove the empty thing if previously is empty
    case mode.Content.Visual(fix, move) =>
      removeInsertionModeIfExists()
      val (min, max) = util.maxMin(fix, move)
      setSelection(IntRange(min, max))
    case mode.Content.Normal(range) =>
      removeInsertionModeIfExists()
      setSelection(range)
  }
}
