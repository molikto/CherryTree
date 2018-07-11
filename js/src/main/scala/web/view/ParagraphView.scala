package web.view

import scalatags.JsDom.all._
import model._
import model.data.{Paragraph, Text}
import org.scalajs.dom._
import org.w3c.dom.css.CSSStyleDeclaration

import scala.scalajs.js

class ParagraphView(init: Paragraph) extends ContentView  {

  private val inserting: ParagraphEditorView = null

  def rec(seq: Seq[model.data.Text]): Seq[Frag] = {
    seq.map {
      case Text.Emphasis(c) => em(rec(c))
      case Text.Strong(c) => strong(rec(c))
      case Text.StrikeThrough(c) => del(rec(c))
      case Text.LaTeX(c) => code(c.toString)
      case Text.Code(c) => code(`class` := "cherrytree-code", c.toString)
      case Text.Plain(c) => span(c.toString)
      case Text.Link(t, b, c) => a(rec(t), href := b.toString)
      case Text.Image(t, b, c) => a(rec(t), href := b.toString)
    }
  }

  dom = p(`class` := "cherrytree-content", rec(init.text)).render

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

  override def clearMode(): Unit = {
    dom.contentEditable = "false"
  }

  override def initMode(): Unit = {
    dom.contentEditable = "true"
  }

  def removeInsertionIfExists(): Unit = {
    // TODO
  }

  def setSelection(min: Int, max: Int): Unit = {
    val range: Range = document.createRange()
    range.setStart(dom, 0)
    range.setEnd(dom, 1)
    val sel = window.getSelection
    sel.removeAllRanges
    sel.addRange(range)
  }

  override def syncMode(aa: mode.Content): Unit =  aa match {
    case mode.Content.Insertion(pos) =>
    case mode.Content.Visual(fix, move) =>
      removeInsertionIfExists()
      val (min, max) = util.maxMin(fix, move)
      setSelection(min, max)
    case mode.Content.Normal(range) =>
      removeInsertionIfExists()
      setSelection(range.start, range.endInclusive)
  }
}
