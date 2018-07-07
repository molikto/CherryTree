package model.data

import scala.collection.mutable.ArrayBuffer


abstract sealed class Text {
  private[model] def serialize(buffer: UnicodeWriter)
  val size: Int
}

/**
  * for now the tree structure is context insensitive
  *
  * context sensitive formats includes no links inside links, etc
  */
object Text {


  def size(paragraph: Seq[Text]): Int = paragraph.map(_.size).sum

  private[model] def parse(reader: UnicodeReader): Text = {
    reader.eatOrNil() match {
      case SpecialChar.EmphasisStart =>
        Emphasis(parse(reader, SpecialChar.EmphasisEnd))
      case SpecialChar.StrongStart =>
        Strong(parse(reader, SpecialChar.StrongEnd))
      case SpecialChar.StrikeThroughStart =>
        StrikeThrough(parse(reader, SpecialChar.StrikeThroughEnd))
      case SpecialChar.LinkStart =>
        Link(parse(reader, SpecialChar.LinkContentEnd), reader.eatUntilAndDrop(SpecialChar.LinkUrlEnd), reader.eatUntilAndDropNonEmpty(SpecialChar.LinkTitleEnd))
      case SpecialChar.ImageStart =>
        Image(parse(reader, SpecialChar.ImageContentEnd), reader.eatUntilAndDrop(SpecialChar.ImageUrlEnd), reader.eatUntilAndDropNonEmpty(SpecialChar.ImageTitleEnd))
      case SpecialChar.CodeStart =>
        Code(reader.eatUntilAndDrop(SpecialChar.CodeEnd))
      case SpecialChar.LaTeXStart =>
        LaTeX(reader.eatUntilAndDrop(SpecialChar.LaTeXEnd))
      case SpecialChar.NotSpecial =>
        Plain(reader.eatUntilSpecialChar())
      case _ =>
        throw new UnicodeParseException("Expecting a non-special char or a special start char")
    }
  }

  private[model] def parse(reader: UnicodeReader, until: SpecialChar.Type): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty && !reader.eatOrFalse(until)) {
      buffer += Text.parse(reader)
    }
    buffer.toVector
  }



  case class Emphasis(content: Seq[Text]) extends Text {
    override val size: Int = Text.size(content) + 2 * SpecialChar.Size

    override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.EmphasisStart)
      content.foreach(_.serialize(buffer))
      buffer.put(SpecialChar.EmphasisEnd)
    }
  }
  case class Strong(content: Seq[Text]) extends Text {
    override val size: Int = Text.size(content)  + 2 * SpecialChar.Size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.StrongStart)
      content.foreach(_.serialize(buffer))
      buffer.put(SpecialChar.StrongEnd)
    }
  }
  case class StrikeThrough(content: Seq[Text]) extends Text {
    override val size: Int = Text.size(content) + 2 * SpecialChar.Size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.StrikeThroughStart)
      content.foreach(_.serialize(buffer))
      buffer.put(SpecialChar.StrikeThroughEnd)
    }
  }
  case class Link(content: Seq[Text], url: Unicode, title: Option[Unicode] = None) extends Text {
    override val size: Int = Text.size(content) + url.size + title.map(_.size).getOrElse(0) + 4 * SpecialChar.Size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.LinkStart)
      content.foreach(_.serialize(buffer))
      buffer.put(SpecialChar.LinkContentEnd)
      buffer.put(url)
      buffer.put(SpecialChar.LinkUrlEnd)
      title.foreach(a => buffer.put(a))
      buffer.put(SpecialChar.LinkTitleEnd)
    }
  }
  case class Image(content: Seq[Text], url: Unicode, title: Option[Unicode] = None) extends Text {
    override val size: Int = Text.size(content) + url.size + title.map(_.size).getOrElse(0) + 4 * SpecialChar.Size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.ImageStart)
      content.foreach(_.serialize(buffer))
      buffer.put(SpecialChar.ImageContentEnd)
      buffer.put(url)
      buffer.put(SpecialChar.ImageUrlEnd)
      title.foreach(a => buffer.put(a))
      buffer.put(SpecialChar.ImageTitleEnd)
    }
  }
  case class Code(unicode: Unicode) extends Text {
    override val size: Int = unicode.size + 2 * SpecialChar.Size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.CodeStart)
      buffer.put(unicode)
      buffer.put(SpecialChar.CodeEnd)
    }
  }
  case class LaTeX(unicode: Unicode) extends Text {
    override val size: Int = unicode.size + 2 * SpecialChar.Size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(SpecialChar.LaTeXStart)
      buffer.put(unicode)
      buffer.put(SpecialChar.LaTeXEnd)
    }
  }
  case class Plain(unicode: Unicode) extends Text {
    override val size: Int = unicode.size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(unicode)
    }
  }
}
