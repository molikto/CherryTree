package shared.operation

import shared._
import shared.operation.Type.Type

abstract sealed class Content extends Operation[model.Content] {
}

object Content {

  object Code {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: model.Content): model.Content = {
        data match {
          case c: model.Content.Code => c.copy(unicode = op(c.unicode))
        }
      }
    }
    case class Lang(lang: Option[String]) extends operation.Content {
      override def ty: Type = Type.AddDelete
      override def apply(data: model.Content): model.Content = {
        data match {
          case c: model.Content.Code => c.copy(lang = lang)
        }
      }
    }
  }
  object Html {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: model.Content): model.Content = {
        data match {
          case c: model.Content.Html => c.copy(unicode = op(c.unicode))
        }
      }
    }
  }
  object LaTeX {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: model.Content): model.Content = {
        data match {
          case c: model.Content.LaTeX => c.copy(unicode = op(c.unicode))
        }
      }
    }
  }
  object Paragraph {
    case class Content(op: operation.Unicode) extends operation.Content {
      override def ty: Type = op.ty
      override def apply(data: model.Content): model.Content = {
        data match {
          case c: model.Content.Paragraph => c.copy(paragraph = model.Text.parse(op(model.Text.serialize(c.paragraph))))
        }
      }
    }
  }
}
