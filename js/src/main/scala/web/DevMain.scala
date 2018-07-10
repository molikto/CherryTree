package web

import org.scalajs.dom
import model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import api.Authentication
import web.view.ClientInitializerView

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


object DevMain {



  @JSExportTopLevel("cherryTreeDevMain")
  def cherryTreeDevMain(id: String): Unit = {
    val mainDiv = el[dom.html.Div](id)
    ScalaComponent.builder.static("TestPanel")(
      ClientInitializerView(Authentication.Token(s"client $id"))
    ).build.apply().renderIntoDOM(mainDiv)
  }
}
