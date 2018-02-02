package client

import client.view._
import org.scalajs.dom
import shared._
import japgolly.scalajs.react._
import shared.data.Authentication
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._


object DevMain {


  def main(args: Array[String]): Unit = {
    val mainDiv = el[dom.html.Div](Ids.main)
    ScalaComponent.builder.static("TestPanel")(
      div(
        width := "100%",
        height := "100%",
        display := "flex",
        flexDirection := "row",
        justifyContent := "center",
        alignItems := "flex-start",
        div(
          width := "40%",
          height := "100%",
          display.`inline-block`,
          ClientInitializerView(Authentication.Token("client 0"))
        ),

        div(
          width := "40%",
          height := "100%",
          display.`inline-block`,
          ClientInitializerView(Authentication.Token("client 1"))
        )
      )
    ).build.apply()
      .renderIntoDOM(mainDiv)
  }
}
