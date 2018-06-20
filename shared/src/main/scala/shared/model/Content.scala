package shared.model

import shared.model


abstract sealed class Content {
}

object Content {
  case class Code(unicode: Unicode, lang: Option[String]) extends Content
  case class Html(unicode: Unicode) extends Content
  case class LaTeX(unicode: Unicode) extends Content
  case class Paragraph(paragraph: model.Paragraph) extends Content
}
