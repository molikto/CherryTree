package model.operation

import model._
import Type.Type

abstract sealed class Content extends Operation[data.Content] {
}

object Content {

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
  object Html {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(d: data.Content): data.Content = {
        d match {
          case c: data.Content.Html => c.copy(unicode = op(c.unicode))
          case _ => throw new AssertionError()
        }
      }
    }
  }
  object LaTeX {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(d: data.Content): data.Content = {
        d match {
          case c: data.Content.LaTeX => c.copy(unicode = op(c.unicode))
          case _ => throw new AssertionError()
        }
      }
    }
  }
  object Paragraph {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(d: data.Content): data.Content = {
        d match {
          case c: data.Content.Paragraph => c.copy(paragraph = data.Paragraph.parse(op(data.Paragraph.serialize(c.paragraph))))
          case _ => throw new AssertionError()
        }
      }
    }
  }
}
