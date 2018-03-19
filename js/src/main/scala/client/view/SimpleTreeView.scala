package client.view


import japgolly.scalajs.react.component.Scala.Unmounted
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import shared.data0.Node


object SimpleTreeView {

  implicit val nodeReuse: Reusability[Node] = Reusability.by_==[Node]

  private val creator = ScalaComponent.builder[Node]("Line")
    .render_P(r => div(p(r.content), ul(r.childs.map(a => li(key := a.id, SimpleTreeView(a))): _*)))
    .configure(Reusability.shouldComponentUpdate)
    .configure(Reusability.shouldComponentUpdateWithOverlay)
    .build

  def apply(a: Node): Unmounted[Node, Unit, Unit] = creator(a)
}

