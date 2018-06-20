package model.operation

import model._
import data.operation.Type.Type

abstract sealed class Content extends Operation[data.Content] {
}

object Content {

  object Code {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: data.Content): data.Content = {
        data match {
          case c: data.Content.Code => c.copy(unicode = op(c.unicode))
        }
      }
    }
    case class Lang(lang: Option[String]) extends operation.Content {
      override def ty: Type = Type.AddDelete
      override def apply(data: data.Content): data.Content = {
        data match {
          case c: data.Content.Code => c.copy(lang = lang)
        }
      }
    }
  }
  object Html {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: data.Content): data.Content = {
        data match {
          case c: data.Content.Html => c.copy(unicode = op(c.unicode))
        }
      }
    }
  }
  object LaTeX {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: data.Content): data.Content = {
        data match {
          case c: data.Content.LaTeX => c.copy(unicode = op(c.unicode))
        }
      }
    }
  }
  object Paragraph {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: data.Content): data.Content = {
        data match {
          case c: data.Content.Paragraph => c.copy(paragraph = data.Text.parse(op(data.Text.serialize(c.paragraph))))
        }
      }
    }
  }
}
