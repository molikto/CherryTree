package model.data

import model.cursor
import model.data.SpecialChar.Type

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
        case SpecialChar.EmphasisStart =>
          Emphasis(parseAll(reader, SpecialChar.EmphasisEnd))
        case SpecialChar.StrongStart =>
          Strong(parseAll(reader, SpecialChar.StrongEnd))
        case SpecialChar.StrikeThroughStart =>
          StrikeThrough(parseAll(reader, SpecialChar.StrikeThroughEnd))
        case SpecialChar.LinkStart =>
          Link(parseAll(reader, SpecialChar.LinkContentEnd), reader.eatUntilAndDrop(SpecialChar.LinkUrlEnd), reader.eatUntilAndDropNonEmpty(SpecialChar.LinkTitleEnd))
        case SpecialChar.ImageStart =>
          Image(parseAll(reader, SpecialChar.ImageContentEnd), reader.eatUntilAndDrop(SpecialChar.ImageUrlEnd), reader.eatUntilAndDropNonEmpty(SpecialChar.ImageTitleEnd))
        case SpecialChar.CodeStart =>
          Code(reader.eatUntilAndDrop(SpecialChar.CodeEnd))
        case SpecialChar.LaTeXStart =>
          LaTeX(reader.eatUntilAndDrop(SpecialChar.LaTeXEnd))
        case _ =>
          throw new UnicodeParseException("Expecting a non-special char or a special start char")
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

  private[model] def parseAll(reader: UnicodeReader, until: SpecialChar.Type): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty && !reader.eatOrFalse(until)) {
      buffer += Text.parse(reader)
    }
    buffer.toVector
  }


  trait Formatted extends Text {
    def content: Seq[Text]
    def styleCharStart: SpecialChar.Type
    def styleCharEnd: SpecialChar.Type

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

  trait Linked extends Formatted {
    def contentEnd: SpecialChar.Type
    def url: Unicode
    def urlEnd: SpecialChar.Type
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
    override def styleCharStart: Type = SpecialChar.EmphasisStart
    override def styleCharEnd: Type = SpecialChar.EmphasisEnd
  }
  case class Strong(override val content: Seq[Text]) extends Formatted {
    override def styleCharStart: Type = SpecialChar.StrongStart
    override def styleCharEnd: Type = SpecialChar.StrongEnd
  }

  case class StrikeThrough(override val content: Seq[Text]) extends Formatted {
    override def styleCharStart: Type = SpecialChar.StrikeThroughStart
    override def styleCharEnd: Type = SpecialChar.StrikeThroughEnd
  }
  case class Link(content: Seq[Text], url: Unicode, title: Option[Unicode] = None) extends Linked {
    override def styleCharStart: Type = SpecialChar.LinkStart
    override def contentEnd: Type = SpecialChar.LinkContentEnd
    override def urlEnd: Type = SpecialChar.LinkUrlEnd
    override def styleCharEnd: Type = SpecialChar.LinkTitleEnd
  }
  case class Image(content: Seq[Text], url: Unicode, title: Option[Unicode] = None) extends Linked {
    override def styleCharStart: Type = SpecialChar.ImageStart
    override def contentEnd: Type = SpecialChar.ImageContentEnd
    override def urlEnd: Type = SpecialChar.ImageUrlEnd
    override def styleCharEnd: Type = SpecialChar.ImageTitleEnd
  }

  trait Coded extends Text {
    def unicode: Unicode
    def start: SpecialChar.Type
    def end: SpecialChar.Type
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
    override def start: Type = SpecialChar.CodeStart
    override def end: Type = SpecialChar.CodeEnd
  }
  case class LaTeX(unicode: Unicode) extends Coded {
    override def start: Type = SpecialChar.LaTeXStart
    override def end: Type = SpecialChar.LaTeXEnd
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
