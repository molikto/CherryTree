package command

import api.ClientUpdate
import client.Client
import model.data.Content
import model.range.IntRange
import model.{ClientState, mode}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

abstract class Command {
  protected val id = ArrayBuffer[String]()
  def name: String = id.mkString(".")
  def defaultKey: String
  def canActOn(a: ClientState): Boolean = Try { action(a) }.isSuccess
  def action(a: ClientState): Client.Update
}

object Command {

  object move {
    abstract class Base extends Command {
      id += "move"
      def mkUpdate(a: mode.Node) = Client.Update(Seq.empty, Some(a), fromUser = true)
    }

    object content {

      abstract class Base extends move.Base {
        id += "content"

        override def canActOn(a: ClientState): Boolean = a.mode match {
          case Some(mode.Node.Content(n, c)) =>
            c match {
              case mode.Content.Normal(r) => super.canActOn(a)
              case mode.Content.Visual(_, _) => super.canActOn(a)
              case _ => false
            }
          case _ => false
        }
        
        def move(content: model.data.Content, a: IntRange): IntRange

        override def action(a: ClientState): Client.Update = a.mode match {
          case Some(o@mode.Node.Content(n, c)) =>
            val content = a.node(n).content
            c match {
              case mode.Content.Normal(r) => mkUpdate(o.copy(a = mode.Content.Normal(move(content, r))))
              case v@mode.Content.Visual(fix, m) =>  mkUpdate(o.copy(a = mode.Content.Visual(fix, move(content, m))))
              case _ => throw new IllegalStateException("")
            }
          case _ => throw new IllegalStateException("")
        }
      }


      // TODO currently we are lazy and not supporting extended grapheme clusters
      val left: Command = new Base() {
        override def defaultKey: String = "h"
        override def move(content: Content, a: IntRange): IntRange = {
          content match {
            case Content.Paragraph(p) =>
              // p.info()
            case Content.Code(u, l) =>
          }
          // TODO
          ???
        }
      }

      val right: Command = new Base() {
        override def defaultKey: String = "l"
        override def move(content: Content, a: IntRange): IntRange = {
          // TODO
          ???
        }
      }
    }


    object node {

    }
  }
}
