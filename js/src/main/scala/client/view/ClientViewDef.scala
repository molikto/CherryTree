package client.view

import shared.data.Authentication
import shared.client.Client
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import shared.Api
import shared.client.ClientInitializer
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

