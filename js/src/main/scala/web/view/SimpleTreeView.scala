package web.view

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import model._


object SimpleTreeView {

  implicit val nodeReuse: Reusability[data.Node] = Reusability.by_==[data.Node]

  private val creator = ScalaComponent.builder[data.Node]("Line")
    // TODO a.hashCode is not a proper id
    .render_P(r => div(p(r.content), ul(r.childs.map(a => li(key := a.hashCode(), SimpleTreeView(a))): _*)))
    .configure(Reusability.shouldComponentUpdate)
    .configure(Reusability.shouldComponentUpdateWithOverlay)
    .build

  def apply(a: data.Node): Unmounted[data.Node, Unit, Unit] = creator(a)
}

