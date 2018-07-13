package model.data

import model.cursor

import scala.collection.mutable.ArrayBuffer


abstract sealed class Text {
  def isEmpty: Boolean

  private[data] def serialize(buffer: UnicodeWriter)
  private[data] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node)
  private[data] def info(a: Int, selfPosition: cursor.Node): Info
  val size: Int
}

/**
  * for now the tree structure is context insensitive
  *
  * context sensitive formats includes no links inside links, etc
  */
object Text {
  // returns null if not inside
  private [data] def info(parentPos: cursor.Node, text: Seq[Text], a: Int): Either[Info, Int] = {
    var totalSize = 0
    var index = 0
    while (index < text.size && text(index).size + totalSize <= a) {
      totalSize += text(index).size
      index += 1
    }
    if (index < text.size) Left(text(index).info(a - totalSize, parentPos :+ index))
    else Right(a - totalSize)
  }


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
        for (i <- 0 until attribute(a).size) buffer += Info(selfPosition, this, InfoType.AttributeUnicode, charPosition = i, specialChar = a)
      })
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = specialCharEnd)
    }

    override def info(start: Int, selfPosition: cursor.Node): Info = {
      var i = start
      if (i == 0) return Info(selfPosition, this, InfoType.Special, specialChar = specialCharStart)
      i -= 1
      Text.info(selfPosition, content, i) match {
        case Left(j) => return j
        case Right(ii) => i = ii
      }
      for (a <- attributes) {
        if (i == 0) return Info(selfPosition, this, InfoType.Special, specialChar = a)
        i -= 1
        if (i < attribute(a).size) return Info(selfPosition, this, InfoType.AttributeUnicode, charPosition = i)
        i -= attribute(a).size
      }
      if (i == 0) return Info(selfPosition, this, InfoType.Special, specialChar = specialCharEnd)
      throw new IllegalArgumentException("index should be smaller in this case")
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
    def specialCharStart: SpecialChar
    def specialCharEnd: SpecialChar
    override val size: Int = unicode.size + 2

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(specialCharStart)
      buffer.put(unicode)
      buffer.put(specialCharEnd)
    }
    override def isEmpty = false

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node): Unit = {
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = specialCharStart)
      for (i <- 0 until unicode.size) buffer += Info(selfPosition, this, InfoType.Coded, charPosition = i)
       buffer += Info(selfPosition, this, InfoType.Special, specialChar = specialCharEnd)
    }
    override def info(start: Int, selfPosition: cursor.Node): Info = {
      var i = start
      if (i == 0) return Info(selfPosition, this, InfoType.Special, specialChar = specialCharStart)
      i -= 1
      if (i < unicode.size) return Info(selfPosition, this, InfoType.Coded, charPosition = i)
      i -= unicode.size
      if (i == 0) return Info(selfPosition, this, InfoType.Special, specialChar = specialCharEnd)
      throw new IllegalArgumentException("index should be smaller in this case")
    }
  }
  case class Code(unicode: Unicode) extends Coded {
    override def specialCharStart: SpecialChar = CodeStart
    override def specialCharEnd: SpecialChar = CodeEnd
  }
  case class LaTeX(unicode: Unicode) extends Coded with AtomicViewed {
    override def specialCharStart: SpecialChar = LaTeXStart
    override def specialCharEnd: SpecialChar = LaTeXEnd
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

    override private[data] def info(a: Int, selfPosition: cursor.Node) =
      Info(selfPosition, this, InfoType.Plain, charPosition = a)
  }
}
