package model.data


abstract sealed class Text {
  val size: Int
}

object Text {

  case class Emphasis(content: Seq[Text]) extends Text {
    override val size: Int = content.map(_.size).sum + 2
  }
  case class Strong(content: Seq[Text]) extends Text {
    override val size: Int = content.map(_.size).sum + 2
  }
  case class StrikeThrough(content: Seq[Text]) extends Text {
    override val size: Int = content.map(_.size).sum + 2
  }
  case class Link(content: Seq[Text], url: Text, title: Option[Text] = None) extends Text {
    override val size: Int = content.size + url.size + title.map(_.size).getOrElse(0) + 4
  }
  case class Image(content: Seq[Text], url: Text, title: Option[Text] = None) extends Text {
    override val size: Int = content.size + url.size + title.map(_.size).getOrElse(0) + 4
  }
  case class Code(unicode: Unicode) extends Text {
    override val size: Int = unicode.size + 2
  }
  case class LaTeX(unicode: Unicode) extends Text {
    override val size: Int = unicode.size + 2
  }
  case class Plain(unicode: Unicode) extends Text {
    override val size: Int = unicode.size
  }
}
