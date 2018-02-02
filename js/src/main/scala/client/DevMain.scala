package client

import client.view._
import org.scalajs.dom
import shared._
import japgolly.scalajs.react._
import shared.data.Authentication


object DevMain {


  def main(args: Array[String]): Unit = {
    val mainDiv = el[dom.html.Div](Ids.main)
    ClientInitializerView(Authentication.Token("client 0")).renderIntoDOM(mainDiv)
  }
}
