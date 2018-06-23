package model.operation

import model._
import Type.Type
import boopickle._

import scala.util.Random

abstract sealed class Content extends Operation[data.Content] {
}

object Content extends OperationObject[data.Content, Content] {

  object Code {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(d: data.Content): data.Content = {
        d match {
          case c: data.Content.Code => c.copy(unicode = op(c.unicode))
          case _ => throw new AssertionError()
        }
      }
    }
    case class Lang(lang: Option[String]) extends operation.Content {
      override def ty: Type = Type.AddDelete
      override def apply(d: data.Content): data.Content = {
        d match {
          case c: data.Content.Code => c.copy(lang = lang)
          case _ => throw new AssertionError()
        }
      }
    }
  }
  object Paragraph {
    case class Content(op: operation.Paragraph) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(d: data.Content): data.Content = {
        d match {
          case c: data.Content.Paragraph => c.copy(paragraph = op(c.paragraph))
          case _ => throw new AssertionError()
        }
      }
    }
  }

  override val pickler: Pickler[Content] = new Pickler[Content] {
    override def pickle(obj: Content)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Content.Code.Content(u) =>
          writeInt(0)
          Unicode.pickler.pickle(u)
        case Content.Code.Lang(l) =>
          writeInt(1)
          writeString(l.getOrElse(""))
        case Content.Paragraph.Content(u) =>
          writeInt(2)
          operation.Paragraph.pickler.pickle(u)
      }
    }

    override def unpickle(implicit state: UnpickleState): Content = {
      import state.dec._
      readInt match {
        case 0 =>
          Content.Code.Content(Unicode.pickler.unpickle)
        case 1 =>
          Content.Code.Lang(readString match {
            case "" => None
            case a => Some(a)
          })
        case 2 =>
          Content.Paragraph.Content(operation.Paragraph.pickler.unpickle)
      }
    }
  }

  override def random(d: data.Content, random: Random): Content = ???
}
