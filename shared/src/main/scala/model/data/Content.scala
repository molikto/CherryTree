package model.data

import model.data


abstract sealed class Content {
}

object Content {
  case class Code(unicode: Unicode, lang: Option[String]) extends Content
  case class Html(unicode: Unicode) extends Content
  case class LaTeX(unicode: Unicode) extends Content
  case class Paragraph(paragraph: data.Paragraph) extends Content {
    val size: Int = paragraph.map(_.size).sum
  }
}
