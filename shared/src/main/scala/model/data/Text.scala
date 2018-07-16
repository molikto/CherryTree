package model.data

import model.cursor
import model.data.SpecialChar.Delimitation

import scala.collection.mutable.ArrayBuffer


abstract sealed class Text {
  def isAtomicViewed: Boolean = this.isInstanceOf[Text.AtomicSelected]

  private[data] def serialize(buffer: UnicodeWriter)
  private[data] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node, selfStart: Int)
  private[data] def info(a: Int, selfPosition: cursor.Node, selfStart: Int): Info
  def size: Int
}

/**
  * for now the tree structure is context insensitive
  *
  * context sensitive formats includes no links inside links, etc
  */
object Text {
  private [data] def info(parentPos: cursor.Node, seqStart: Int, text: Seq[Text], buffer: ArrayBuffer[Info]): Unit = {
    var pos = seqStart
    var index = 0
    for (t <- text) {
      t.info(buffer, parentPos :+ index, pos)
      pos += t.size
      index += 1
    }
  }

  // returns null if not inside
  private [data] def info(parentPos: cursor.Node,
    seqStart: Int,
    text: Seq[Text],
    a: Int): Info = {
    var ss = seqStart
    var totalSize = 0
    var index = 0
    while (text(index).size + totalSize <= a) {
      val sss = text(index).size
      totalSize += sss
      ss += sss
      index += 1
    }
    text(index).info(a - totalSize, parentPos :+ index, ss)
  }


  sealed trait AtomicSelected extends Text {

  }

  def size(content: Seq[Text]): Int = content.map(_.size).sum

  private[model] def parse(reader: UnicodeReader): Text = {
    reader.eatOrNotSpecial() match {
      case Some(a) => a match { // LATER generic parser
        case EmphasisStart =>
          Emphasis(parseAll(reader, EmphasisEnd))
        case StrongStart =>
          Strong(parseAll(reader, StrongEnd))
        case StrikeThroughStart =>
          StrikeThrough(parseAll(reader, StrikeThroughEnd))
        case LinkStart =>
          Link(parseAll(reader, UrlAttribute), reader.eatUntilAndDrop(TitleAttribute), reader.eatUntilAndDrop(LinkEnd))
        case ImageStart =>
          reader.eat(UrlAttribute)
          Image(reader.eatUntilAndDrop(TitleAttribute), reader.eatUntilAndDrop(ImageEnd))
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

  sealed trait Delimited[T] extends Text {
    def content: T
    def contentSize: Int
    private[model] def serializeContent(buffer: UnicodeWriter): Unit
    private[model] def contentInfo(buffer: ArrayBuffer[Info], selfPosition: cursor.Node, selfStart: Int, contentStart: Int): Unit
    private [model] def contentInfo(selfPosition: cursor.Node, selfStart: Int, a: Int, contentStart: Int): Info

    final private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(delimitation.start)
      serializeContent(buffer)
      attributes.foreach(a => {
        buffer.put(a)
        buffer.put(attribute(a))
      })
      buffer.put(delimitation.end)
    }

    def delimitation: SpecialChar.Delimitation

    def attributes: Seq[SpecialChar] = delimitation.attributes
    def attribute(i: _root_.model.data.SpecialChar): Unicode = throw new NotImplementedError()

    final override lazy val size: Int = 2 + contentSize + attributes.map(a => attribute(a).size + 1).sum

    final override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node, selfStart: Int): Unit = {
      var position = selfStart
      buffer += Info(selfPosition, selfStart, this, InfoType.Special, position, specialChar = delimitation.start)
      position += 1
      contentInfo(buffer, selfPosition, selfStart, position)
      position += contentSize
      attributes.foreach(a => {
        buffer += Info(selfPosition, selfStart, this, InfoType.Special, position, specialChar = a)
        position += 1
        attribute(a).codePoints.forEach(c => {
          buffer += Info(selfPosition, selfStart, this, InfoType.AttributeUnicode, position, char = c, specialChar = a)
          position += 1
        })
      })
      buffer += Info(selfPosition, selfStart, this, InfoType.Special, position, specialChar = delimitation.end)
    }

    final override def info(start: Int, selfPosition: cursor.Node, selfStart: Int): Info = {
      val infoPosition = selfStart + start
      var i = start
      if (i == 0) return Info(selfPosition, selfStart, this, InfoType.Special, infoPosition, specialChar = delimitation.start)
      i -= 1
      if (i < contentSize) return contentInfo(selfPosition, selfStart, i, selfStart + 1)
      i -= contentSize
      for (a <- attributes) {
        if (i == 0) return Info(selfPosition, selfStart, this, InfoType.Special, infoPosition, specialChar = a)
        i -= 1
        val attr = attribute(a)
        if (i < attr.size) return Info(selfPosition, selfStart, this, InfoType.AttributeUnicode, infoPosition, char = attr(i))
        i -= attribute(a).size
      }
      if (i == 0) return Info(selfPosition, selfStart, this, InfoType.Special, infoPosition, specialChar = delimitation.end)
      throw new IllegalArgumentException("Out of bound")
    }
  }

  sealed private[model] trait Formatted extends Delimited[Seq[Text]] {
    def content: Seq[Text]
    def delimitation: SpecialChar.Delimitation
    lazy val contentSize: Int = Text.size(content)

    override private[model] def serializeContent(buffer: UnicodeWriter): Unit = {
      content.foreach(_.serialize(buffer))
    }

    override private[model] def contentInfo(buffer: ArrayBuffer[Info], selfPosition: cursor.Node, selfStart: Int, contentStart: Int): Unit = {
      Text.info(selfPosition, contentStart, content, buffer)
    }

    override private[model] def contentInfo(selfPosition: cursor.Node, selfStart: Int, a: Int, contentStart: Int): Info = {
      Text.info(selfPosition, contentStart, content, a)
    }
  }

  case class Emphasis(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Emphasis
  }
  case class Strong(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Strong
  }

  case class StrikeThrough(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.StrikeThrough
  }
  case class Link(content: Seq[Text], url: Unicode, title: Unicode = Unicode.empty) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Link
    override def attribute(i: SpecialChar): Unicode = if (i == UrlAttribute) url else title
  }
  case class Image(url: Unicode, title: Unicode = Unicode.empty) extends Formatted with AtomicSelected {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Image
    override def attribute(i: SpecialChar): Unicode = if (i == UrlAttribute) url else title
    override def content: Seq[Text] = Seq.empty
  }

  sealed trait Coded extends Delimited[Unicode] {
    def content: Unicode
    def delimitation: SpecialChar.Delimitation
    override def contentSize: Int = content.size

    override private[model] def serializeContent(buffer: UnicodeWriter): Unit = {
      buffer.put(content)
    }

    override private[model] def contentInfo(buffer: ArrayBuffer[Info], selfPosition: cursor.Node, selfStart: Int, contentStart: Int): Unit = {
      var position = contentStart
      content.codePoints.forEach(c => {
        buffer += Info(selfPosition, selfStart, this, InfoType.Coded, position, char = c)
        position += 1
      })
    }

    override private [model] def contentInfo(selfPosition: cursor.Node, selfStart: Int, a: Int, contentStart: Int): Info = {
      Info(selfPosition, selfStart, this, InfoType.Coded, contentStart + a, char = content(a))
    }
  }
  case class Code(content: Unicode) extends Coded {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Code
  }
  case class LaTeX(content: Unicode) extends Coded with AtomicSelected {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.LaTeX
  }

  /**
    * we make it invariant tha plain cannot be empty
    */
  case class Plain(unicode: Unicode) extends Text {
    assert(!unicode.isEmpty)
    override def size: Int = unicode.size

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(unicode)
    }

    override private[model] def info(buffer: ArrayBuffer[Info], selfPosition: cursor.Node, selfStart: Int): Unit = {
      var position = selfStart
      unicode.codePoints.forEach(c => {
        buffer += Info(selfPosition, selfStart, this, InfoType.Plain, position, char = c)
        position += 1
      })
    }

    override private[data] def info(a: Int, selfPosition: cursor.Node, selfStart: Int) = {
      Info(selfPosition, selfStart, this, InfoType.Plain, a + selfStart, char = unicode(a))
    }
  }
}
