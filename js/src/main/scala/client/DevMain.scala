package client

import org.scalajs.dom
import shared._

object DevMain {


  def main(args: Array[String]): Unit = {
    val mainDiv = el[dom.html.Div](Ids.main)
    val client = new Client(mainDiv)
  }
}
