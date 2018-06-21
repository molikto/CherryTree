package model.data

import boopickle._
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

  val pickler: Pickler[Content] = new Pickler[Content] {
    override def pickle(obj: Content)(implicit state: PickleState): Unit = {

    }

    override def unpickle(implicit state: UnpickleState): Content = ???
  }
}
