package client.view


import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.{BackendScope, Unmounted}
import shared.client._
import shared.data.ClientState
import shared.client.Client
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import shared.Api
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object SimpleTreeView {


  private val creator = ScalaComponent.builder[Node]("Line")
    .render_P(r => div(p(r.content), ol(r.childs.map(a => li(SimpleTreeView(a))): _*)))
    .build

  def apply(a: Node): Unmounted[Node, Unit, Unit] = creator(a)
}
