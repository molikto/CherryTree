package model.data

import model.cursor

import scala.collection.mutable.ArrayBuffer


abstract sealed class Text {
  def isEmpty: Boolean

  private[data] def serialize(buffer: UnicodeWriter)
  private[data] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node)
  val size: Int
}

/**
  * for now the tree structure is context insensitive
  *
  * context sensitive formats includes no links inside links, etc
  */
object Text {

  sealed trait AtomicViewed extends Text {

  }

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
          Link(parseAll(reader, UrlAttribute), reader.eatUntilAndDrop(TitleAttribute), reader.eatUntilAndDrop(LinkEnd))
        case ImageStart =>
          Image(parseAll(reader, UrlAttribute), reader.eatUntilAndDrop(TitleAttribute), reader.eatUntilAndDrop(ImageEnd))
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

  sealed private[model] trait Formatted extends Text {
    def content: Seq[Text]
    def specialCharStart: SpecialChar
    def specialCharEnd: SpecialChar
    override def isEmpty = false
    def attributes: Seq[SpecialChar] = Seq.empty
    def attribute(i: _root_.model.data.SpecialChar): Unicode = throw new IllegalArgumentException()

    override val size: Int = 2 + Text.size(content) + attributes.map(a => attribute(a).size + 1).sum

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(specialCharStart)
      content.foreach(_.serialize(buffer))
      attributes.foreach(a => {
        buffer.put(a)
        buffer.put(attribute(a))
      })
      buffer.put(specialCharEnd)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = specialCharStart)
      content.zipWithIndex.foreach(a => a._1.info(buffer, selfPosition :+ a._2))
      attributes.foreach(a => {
        buffer += Info(selfPosition, this, InfoType.Special, specialChar = a)
        for (i <- 0 until attribute(a).size) buffer += Info(selfPosition, this, InfoType.AttributeUnicode, charPosition = i)
      })
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = specialCharEnd)
    }
  }

  case class Emphasis(override val content: Seq[Text]) extends Formatted {
    override def specialCharStart: SpecialChar = EmphasisStart
    override def specialCharEnd: SpecialChar = EmphasisEnd

  }
  case class Strong(override val content: Seq[Text]) extends Formatted {
    override def specialCharStart: SpecialChar = StrongStart
    override def specialCharEnd: SpecialChar = StrongEnd
  }

  case class StrikeThrough(override val content: Seq[Text]) extends Formatted {
    override def specialCharStart: SpecialChar = StrikeThroughStart
    override def specialCharEnd: SpecialChar = StrikeThroughEnd
  }
  case class Link(content: Seq[Text], url: Unicode, title: Unicode = Unicode.empty) extends Formatted {
    override def specialCharStart: SpecialChar = LinkStart
    override def specialCharEnd: SpecialChar = LinkEnd
    override def attributes: Seq[SpecialChar] = Seq(UrlAttribute, TitleAttribute)
    override def attribute(i: SpecialChar): Unicode = if (i == UrlAttribute) url else title
  }
  case class Image(content: Seq[Text], url: Unicode, title: Unicode = Unicode.empty) extends Formatted with AtomicViewed {
    override def specialCharStart: SpecialChar = ImageStart
    override def specialCharEnd: SpecialChar = ImageEnd
    override def attributes: Seq[SpecialChar] = Seq(UrlAttribute, TitleAttribute)
    override def attribute(i: SpecialChar): Unicode = if (i == UrlAttribute) url else title
  }

  sealed trait Coded extends Text {
    def unicode: Unicode
    def start: SpecialChar
    def end: SpecialChar
    override val size: Int = unicode.size + 2

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(start)
      buffer.put(unicode)
      buffer.put(end)
    }
    override def isEmpty = false

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = start)
      for (i <- 0 until unicode.size) buffer += Info(selfPosition, this, InfoType.Coded, charPosition = i)
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = end)
    }
  }
  case class Code(unicode: Unicode) extends Coded {
    override def start: SpecialChar = CodeStart
    override def end: SpecialChar = CodeEnd
  }
  case class LaTeX(unicode: Unicode) extends Coded with AtomicViewed {
    override def start: SpecialChar = LaTeXStart
    override def end: SpecialChar = LaTeXEnd
  }
  case class Plain(unicode: Unicode) extends Text {
    override val size: Int = unicode.size
    override def isEmpty: Boolean = unicode.isEmpty

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(unicode)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
      for (i <- 0 until unicode.size) buffer += Info(selfPosition, this, InfoType.Plain, charPosition = i)
    }
  }
}
