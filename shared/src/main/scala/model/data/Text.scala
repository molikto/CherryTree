package model.data

import model.cursor

import scala.collection.mutable.ArrayBuffer


abstract sealed class Text {

  private[model] def serialize(buffer: UnicodeWriter)
  private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node)
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
    reader.eatOrNotSpecial() match {
      case Some(a) => a match {
        case EmphasisStart =>
          Emphasis(parseAll(reader, EmphasisEnd))
        case StrongStart =>
          Strong(parseAll(reader, StrongEnd))
        case StrikeThroughStart =>
          StrikeThrough(parseAll(reader, StrikeThroughEnd))
        case LinkStart =>
          Link(parseAll(reader, LinkContentEnd), reader.eatUntilAndDrop(LinkUrlEnd), reader.eatUntilAndDropNonEmpty(LinkTitleEnd))
        case ImageStart =>
          Image(parseAll(reader, ImageContentEnd), reader.eatUntilAndDrop(ImageUrlEnd), reader.eatUntilAndDropNonEmpty(ImageTitleEnd))
        case CodeStart =>
          Code(reader.eatUntilAndDrop(CodeEnd))
        case LaTeXStart =>
          LaTeX(reader.eatUntilAndDrop(LaTeXEnd))
        case kk =>
          throw new UnicodeParseException(s"Expecting a non-special char or a special start char, but found $kk")
      }
      case None =>
        Plain(reader.eatUntilSpecialChar())
    }
  }

  private[model] def parseAll(reader: UnicodeReader): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty) {
      buffer += Text.parse(reader)
    }
    buffer.toVector
  }

  private[model] def parseAll(reader: UnicodeReader, until: SpecialChar): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty && !reader.eatOrFalse(until)) {
      buffer += Text.parse(reader)
    }
    buffer.toVector
  }


  private[model] trait Formatted extends Text {
    def content: Seq[Text]
    def styleCharStart: SpecialChar
    def styleCharEnd: SpecialChar

    override val size: Int = Text.size(content) + 2

    override private[model] def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(styleCharStart)
      content.foreach(_.serialize(buffer))
      buffer.put(styleCharEnd)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
       buffer += Info(selfPosition, this, InfoType.Special(styleCharStart))
      content.zipWithIndex.foreach(a => a._1.info(buffer, selfPosition :+ a._2))
       buffer += Info(selfPosition, this, InfoType.Special(styleCharEnd))
    }
  }

  private[model] trait Linked extends Formatted {
    def contentEnd: SpecialChar
    def url: Unicode
    def urlEnd: SpecialChar
    def title: Option[Unicode]
    override val size: Int = Text.size(content) + url.size + title.map(_.size).getOrElse(0) + 4

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(styleCharStart)
      content.foreach(_.serialize(buffer))
      buffer.put(contentEnd)
      buffer.put(url)
      buffer.put(urlEnd)
      title.foreach(a => buffer.put(a))
      buffer.put(styleCharEnd)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
       buffer += Info(selfPosition, this, InfoType.Special(styleCharStart))
      content.zipWithIndex.foreach(a => a._1.info(buffer, selfPosition :+ a._2))
       buffer += Info(selfPosition, this, InfoType.Special(contentEnd))
      for (_ <- 0 until url.size) buffer += Info(selfPosition, this, InfoType.Unicode)
       buffer += Info(selfPosition, this, InfoType.Special(urlEnd))
      for (_ <- 0 until title.map(_.size).getOrElse(0)) buffer += Info(selfPosition, this, InfoType.Unicode)
       buffer += Info(selfPosition, this, InfoType.Special(styleCharEnd))
    }
  }

  case class Emphasis(override val content: Seq[Text]) extends Formatted {
    override def styleCharStart: SpecialChar = EmphasisStart
    override def styleCharEnd: SpecialChar = EmphasisEnd
  }
  case class Strong(override val content: Seq[Text]) extends Formatted {
    override def styleCharStart: SpecialChar = StrongStart
    override def styleCharEnd: SpecialChar = StrongEnd
  }

  case class StrikeThrough(override val content: Seq[Text]) extends Formatted {
    override def styleCharStart: SpecialChar = StrikeThroughStart
    override def styleCharEnd: SpecialChar = StrikeThroughEnd
  }
  case class Link(content: Seq[Text], url: Unicode, title: Option[Unicode] = None) extends Linked {
    override def styleCharStart: SpecialChar = LinkStart
    override def contentEnd: SpecialChar = LinkContentEnd
    override def urlEnd: SpecialChar = LinkUrlEnd
    override def styleCharEnd: SpecialChar = LinkTitleEnd
  }
  case class Image(content: Seq[Text], url: Unicode, title: Option[Unicode] = None) extends Linked {
    override def styleCharStart: SpecialChar = ImageStart
    override def contentEnd: SpecialChar = ImageContentEnd
    override def urlEnd: SpecialChar = ImageUrlEnd
    override def styleCharEnd: SpecialChar = ImageTitleEnd
  }

  trait Coded extends Text {
    def unicode: Unicode
    def start: SpecialChar
    def end: SpecialChar
    override val size: Int = unicode.size + 2

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(start)
      buffer.put(unicode)
      buffer.put(end)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
       buffer += Info(selfPosition, this, InfoType.Special(start))
      for (_ <- 0 until unicode.size) buffer += Info(selfPosition, this, InfoType.Coded)
       buffer += Info(selfPosition, this, InfoType.Special(end))
    }
  }
  case class Code(unicode: Unicode) extends Coded {
    override def start: SpecialChar = CodeStart
    override def end: SpecialChar = CodeEnd
  }
  case class LaTeX(unicode: Unicode) extends Coded {
    override def start: SpecialChar = LaTeXStart
    override def end: SpecialChar = LaTeXEnd
  }
  case class Plain(unicode: Unicode) extends Text {
    override val size: Int = unicode.size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(unicode)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
      for (_ <- 0 until unicode.size) buffer += Info(selfPosition, this, InfoType.Plain)
    }
  }
}
