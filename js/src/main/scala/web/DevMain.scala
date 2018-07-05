package web

import controller.Ids
import org.scalajs.dom
import model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import controller.api.Authentication
import web.view.ClientInitializerView


object DevMain {


  def main(args: Array[String]): Unit = {
    val mainDiv = el[dom.html.Div](Ids.main)
    mainDiv.contentEditable = "true"
//    ScalaComponent.builder.static("TestPanel")(
//      div(
//        width := "100%",
//        height := "100%",
//        display := "flex",
//        flexDirection := "row",
//        justifyContent := "center",
//        alignItems := "flex-start",
//        div(
//          width := "40%",
//          height := "100%",
//          display.`inline-block`,
//          ClientInitializerView(Authentication.Token("client 0"))
//        ),
//
//        div(
//          width := "40%",
//          height := "100%",
//          display.`inline-block`,
//          ClientInitializerView(Authentication.Token("client 1"))
//        )
//      )
//    ).build.apply().renderIntoDOM(mainDiv)
  }
}
