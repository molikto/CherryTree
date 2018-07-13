package command

import api.ClientUpdate
import client.Client
import model.ClientState

import scala.collection.mutable.ArrayBuffer

abstract class Command {
  val id = ArrayBuffer[String]()
  def defaultKey: String
  def action(a: ClientState): Client.Update
}

object command {
  object move {
    abstract class Base extends Command {
      id += "move"
    }
    object content {
      abstract class Base extends move.Base {
        id += "content"

      }
      val left = new Base() {
        override def defaultKey: String = ???

        override def action(a: ClientState): Client.Update = ???
      }
    }


    object node {

    }
  }
}
