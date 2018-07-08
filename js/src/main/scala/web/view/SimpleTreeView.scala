package web.view

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.VdomNode
import model._
import model.data
import model.data._
import model.data.Content


object SimpleTreeView {

  implicit val nodeReuse: Reusability[data.Node] = Reusability.by_==[data.Node]

  def toHtml(seq: Seq[Text]): VdomNode  = seq.toVdomArray(toHtml)

  def toHtml(text: data.Text): VdomNode = text match {
    case Text.Emphasis(c) => em(toHtml(c))
    case Text.Strong(c) => strong(toHtml(c))
    case Text.StrikeThrough(c) => del(toHtml(c))
    case Text.LaTeX(c) => code(c.toString)
    case Text.Code(c) => code(c.toString)
    case Text.Plain(c) => span(c.toString)
    case Text.Link(t, b, c) => a(toHtml(t), href := b.toString)
    case Text.Image(t, b, c) => a(toHtml(t), href := b.toString)
  }

  def toHtml(content: Content): VdomNode = content match {
    case Content.Code(u, l) => pre(u.toString)
    case Content.Paragraph(a) => p(toHtml(a.text))
  }

  private val creator = ScalaComponent.builder[data.Node]("Line")
    // LATER a.hashCode is not a proper id
    .render_P(r => div(p(toHtml(r.content)), ul(r.childs.map(a => li(key := a.hashCode(), SimpleTreeView(a))): _*)))
    .configure(Reusability.shouldComponentUpdate)
    .configure(Reusability.shouldComponentUpdateWithOverlay)
    .build

  def apply(a: data.Node): Unmounted[data.Node, Unit, Unit] = creator(a)
}

