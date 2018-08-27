package model.data

import model.cursor
import model.data.SpecialChar.{Delimitation, DelimitationType}
import model.range.IntRange
import scalatags.Text
import settings.SpecialKeySettings

import scala.collection.mutable.ArrayBuffer
import scalatags.Text.all._


abstract sealed class Text {
  def toScalaTags: Frag
  def toPlainScalaTags: Frag

  def apply(cur: model.cursor.Node): Text = if (cur.isEmpty) this else throw new NotImplementedError()
  def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean = false

  def isAtomic: Boolean = this.isInstanceOf[Text.Atomic]
  def isCoded: Boolean = this.isInstanceOf[Text.Coded]
  def asCoded: Text.Coded = this.asInstanceOf[Text.Coded]
  def isCodedNonAtomic: Boolean = this.isInstanceOf[Text.Coded] && asCoded.delimitation.codedNonAtomic
  def isCodedAtomic: Boolean = this.isInstanceOf[Text.Coded] && asCoded.delimitation.codedAtomic
  def isPlain: Boolean = this.isInstanceOf[Text.Plain]
  def isDelimited: Boolean = this.isInstanceOf[Text.Delimited]
  def asDelimited: Text.Delimited = this.asInstanceOf[Text.Delimited]
  def asPlain: Text.Plain = this.asInstanceOf[Text.Plain]

  private[data] def serialize(buffer: UnicodeWriter)
  def size: Int

  def after(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom]
  def atoms(myCursor: model.cursor.Node, myIndex: Int): Iterator[Atom] = after(myCursor, myIndex, 0)
  def before(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom]
}

/**
  * for now the tree structure is context insensitive
  *
  * context sensitive formats includes no links inside links, etc
  */
object Text {

  private[data] def toScalaTags(a: Seq[Text]): Frag = a.map(_.toScalaTags): Frag
  private[data] def toPlainScalaTags(a: Seq[Text]): Frag = a.map(_.toPlainScalaTags): Frag
  def toHtml(a: Seq[Text]): String = toScalaTags(a).render
  def toPlain(a: Seq[Text]): String = toPlainScalaTags(a).render

  def quickSearch(text: Seq[Text], p: Unicode, deli: SpecialKeySettings): Boolean = {
    text.exists(_.quickSearch(p, deli))
  }


  private[model] def serialize(text: Seq[Text]): Unicode = {
    val buffer = new UnicodeWriter()
    text.foreach(_.serialize(buffer))
    buffer.toUnicode
  }


  def assemble(atoms: Seq[Atom]): Seq[Text] = {
    val buffer = new UnicodeWriter()
    atoms.foreach(_.serialize(buffer))
    Text.parseAll(new UnicodeReader(buffer.toUnicode))
  }

  private[data] def before(myCursor: model.cursor.Node, myIndex: Int, b: Int, a: Seq[Text]) =
    if (a.isEmpty) Iterator.empty
    else {
      new Iterator[Atom] {
        private var i = 0 // candidate
        private var it: Iterator[Atom] = null
        var j = 0

        {
          var size = a.head.size
          while (i < a.size && j + size <= b) {
            j += size
            i += 1
            if (i < a.size) size = a(i).size
          }
          if (i == a.size) {
            i -= 1
            j -= a.last.size
          }
          it = a(i).before(myCursor :+ i, j + myIndex, b - j)
        }

        override def hasNext: Boolean = i > 0 || it.hasNext

        override def next(): Atom = {
          if (!it.hasNext) {
            i -= 1
            j -= a(i).size
            it = a(i).before(myCursor :+ i, j + myIndex, a(i).size)
          }
          it.next() // we always know this is not empty
        }
      }
    }

  private[data] def after(myCursor: model.cursor.Node, myIndex: Int, b: Int, a: Seq[Text]) = if (a.isEmpty) Iterator.empty
  else {
    new Iterator[Atom] {
      private var i = 0 // candidate
      private var it: Iterator[Atom] = null

      var j = 0

      {
        while (i < a.size && j + a(i).size <= b) {
          j += a(i).size
          i += 1
        }
        if (i < a.size) {
          it = a(i).after(myCursor :+ i, j + myIndex, b - j)
          assert(it != null)
        }
      }

      override def hasNext: Boolean = i != a.size && (i < a.size - 1 || it.hasNext)

      override def next(): Atom = {
        if (!it.hasNext) {
          j += a(i).size
          i += 1
          it = a(i).atoms(myCursor :+ i, j + myIndex)
        }
        it.next() // we always know this is not empty
      }
    }
  }

  sealed trait Atomic extends Text {
    final override def after(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = {
      if (i < size) Iterator.single(Atom.Marked(myCursor, myIndex, this))
      else Iterator.empty
    }

    override def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean = false

    final override def before(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = {
      if (i == size)
        Iterator.single(Atom.Marked(myCursor, myIndex, this))
      else Iterator.empty
    }
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
        case HTMLStart =>
          HTML(reader.eatUntilAndDrop(HTMLEnd))
        case kk =>
          throw new UnicodeParseException(s"Expecting a non-special char or a special start char, but found $kk, reader:\n$reader")
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

  sealed trait Delimited extends Text {
    def contentSize: Int
    private[model] def serializeContent(buffer: UnicodeWriter): Unit
    private[model] def serializeAttributes(buffer: UnicodeWriter): Unit  = {
      attributes.foreach(a => {
        buffer.put(a)
        buffer.put(attribute(a))
      })
    }

    final private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(delimitation.start)
      serializeContent(buffer)
      serializeAttributes(buffer)
      buffer.put(delimitation.end)
    }

    def delimitation: SpecialChar.Delimitation

    def attributes: Seq[SpecialChar] = delimitation.attributes
    def attribute(i: SpecialChar): Unicode = throw new NotImplementedError()
    def urlAttr: Unicode = attribute(UrlAttribute)
    def titleAttr: Unicode = attribute(TitleAttribute)
    def rangeAttribute(i: SpecialChar): IntRange = {
      val skip = 1 + contentSize + attributes.takeWhile(_ != i).map(a => attribute(a).size + 1).sum + 1
      IntRange(skip, skip + attribute(i).size)
    }

    def deliEndSize = skipSize + 1
    def skipSize: Int = attributes.map(a => attribute(a).size + 1).sum

    final override lazy val size: Int = 2 + contentSize + skipSize
  }

  sealed trait DelimitedT[T] extends Delimited {
    def content: T

    override def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean =
      p == deli(delimitation.start) || p == deli(delimitation.end)
  }

  sealed trait Formatted extends DelimitedT[Seq[Text]] {
    def content: Seq[Text]
    def delimitation: SpecialChar.Delimitation
    lazy val contentSize: Int = Text.size(content)
    override def apply(cur: model.cursor.Node): Text = if (cur.isEmpty) this else content(cur.head)(cur.tail)

    override def toPlainScalaTags: Frag = Text.toScalaTags(content)

    override def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean =
      super.quickSearch(p, deli) ||
      Text.quickSearch(content, p, deli)

    override private[model] def serializeContent(buffer: UnicodeWriter): Unit = {
      content.foreach(_.serialize(buffer))
    }

    override def after(myCursor: model.cursor.Node, myIndex: Int, b: Int): Iterator[Atom] = new Iterator[Atom] {
      var i = if (b == 0) 0 else if (b != Formatted.this.size) 1 else 2
      var it: Iterator[Atom] = null
      override def hasNext: Boolean = i < 2

      override def next(): Atom = if (i == 0) {
        i += 1
        Atom.FormattedSpecial(myCursor, myIndex, delimitation.start, Formatted.this)
      } else if (i == 1) {
        if (it == null) it = Text.after(myCursor, myIndex + 1, (b - 1) max 0, content)
        if (it.hasNext) {
          it.next()
        } else {
          i = 2
          Atom.FormattedSpecial(myCursor, myIndex + 1 + contentSize, delimitation.end, Formatted.this)
        }
      } else {
        throw new IllegalArgumentException("No more")
      }
    }

    override def before(myCursor: model.cursor.Node, myIndex: Int, b: Int): Iterator[Atom] = new Iterator[Atom] {
      var i = if (b == Formatted.this.size) 2 else if (b > 1) 1 else if (b > 0) 0 else -1
      var it: Iterator[Atom] = null
      override def hasNext: Boolean = i >= 0

      override def next(): Atom = if (i == 2) {
        i -= 1
        Atom.FormattedSpecial(myCursor, myIndex + 1 + contentSize, delimitation.end, Formatted.this)
      } else if (i == 1) {
        if (it == null) it = Text.before(myCursor, myIndex + 1, ((b - 1) max 0) min contentSize, content)
        if (it.hasNext) {
          it.next()
        } else {
          i = -1
          Atom.FormattedSpecial(myCursor, myIndex, delimitation.start, Formatted.this)
        }
      } else if (i == 0) {
        i = -1
        Atom.FormattedSpecial(myCursor, myIndex, delimitation.start, Formatted.this)
      } else {
        throw new IllegalArgumentException("No more")
      }
    }
  }

  case class Emphasis(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Emphasis

    override def toScalaTags: Frag = em(Text.toScalaTags(content))
  }
  case class Strong(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Strong
    override def toScalaTags: Frag = strong(Text.toScalaTags(content))
  }

  case class StrikeThrough(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.StrikeThrough
    override def toScalaTags: Frag = del(Text.toScalaTags(content))
  }
  case class Link(content: Seq[Text], url: Unicode, tit: Unicode = Unicode.empty) extends Formatted {
    def isNodeRef: Boolean = url.str.startsWith(Node.NodeRefScheme)
    override def toScalaTags: Frag = a(Text.toScalaTags(content), href := url.str, title := tit.str)
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content), s" (${url.str})": Frag)

    override def delimitation: SpecialChar.Delimitation = SpecialChar.Link
    override def attribute(i: SpecialChar): Unicode =
      if (i == UrlAttribute) url
      else if (i == TitleAttribute) tit
      else throw new IllegalArgumentException("Not here")
  }

  sealed trait Coded extends DelimitedT[Unicode] {
    def content: Unicode
    def delimitation: SpecialChar.Delimitation
    override def contentSize: Int = content.size

    //override def apply(cur: model.cursor.Node): Text = if (cur.isEmpty) this else if (cur == Se

    override private[model] def serializeContent(buffer: UnicodeWriter): Unit = {
      buffer.put(content)
    }

    override def quickSearch(p: Unicode, deli: settings.SpecialKeySettings): Boolean =
      super.quickSearch(p, deli) || content.containsLowerCase(p)

    override def after(myCursor: model.cursor.Node, myIndex: Int, b: Int): Iterator[Atom] = new Iterator[Atom] {
      var i = if (b == 0) 0 else if (b != Coded.this.size) 1 else 2
      var it: Iterator[Atom] = null
      override def hasNext: Boolean = i < 2

      override def next(): Atom = if (i == 0) {
        i += 1
        Atom.CodedSpecial(myCursor, myIndex, delimitation.start, Coded.this)
      } else if (i == 1) {
        if (it == null) it = content.after((b - 1) max 0).map(a => Atom.CodedGrapheme(myCursor :+ 0, myIndex + 1 + a._1, a._1, a._2, Coded.this))
        if (it.hasNext) {
          it.next()
        } else {
          i = 2
          Atom.CodedSpecial(myCursor, myIndex + 1 + contentSize, delimitation.end, Coded.this)
        }
      } else {
        throw new IllegalArgumentException("No more")
      }
    }


    override def before(myCursor: model.cursor.Node, myIndex: Int, b: Int): Iterator[Atom] = new Iterator[Atom] {
      var i = if (b == Coded.this.size) 2 else if (b > 1) 1 else if (b > 0) 0 else -1
      var it: Iterator[Atom] = null
      override def hasNext: Boolean = i >= 0

      override def next(): Atom = if (i == 2) {
        i -= 1
        Atom.CodedSpecial(myCursor, myIndex + 1 + contentSize, delimitation.end, Coded.this)
      } else if (i == 1) {
        if (it == null) it = content.before(((b - 1) max 0) min contentSize).map(a => Atom.CodedGrapheme(myCursor :+ 0, a._1 + myIndex + 1, a._1, a._2, Coded.this))
        if (it.hasNext) {
          it.next()
        } else {
          i = -1
          Atom.CodedSpecial(myCursor, myIndex, delimitation.start, Coded.this)
        }
      } else if (i == 0) {
        i = -1
        Atom.CodedSpecial(myCursor, myIndex, delimitation.start, Coded.this)
      } else {
        throw new IllegalArgumentException("No more")
      }
    }


  }
  case class Code(content: Unicode) extends Coded {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Code

    override def toScalaTags: Frag = code(content.str)

    override def toPlainScalaTags: Frag = content.str
  }
  case class LaTeX(content: Unicode) extends Coded with Atomic {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.LaTeX

    override def toScalaTags: Frag = code(`class` := "latex", content.str)

    override def toPlainScalaTags: Frag = content.str
  }

  case class HTML(content: Unicode) extends Coded with Atomic {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.HTML
    override def toScalaTags: Frag = raw(content.str)

    override def toPlainScalaTags: Frag = toScalaTags
  }


  sealed trait DelimitedEmpty extends DelimitedT[Unit] {
    override def content: Unit = Unit
    override def contentSize: Int = 0
    override private[model] def serializeContent(buffer: UnicodeWriter): Unit = {}
  }


  case class Image(url: Unicode, tit: Unicode = Unicode.empty) extends DelimitedEmpty with Atomic {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Image
    override def attribute(i: SpecialChar): Unicode = if (i == UrlAttribute) url else if (i == TitleAttribute) tit else throw new IllegalArgumentException("Not here")

    override def toScalaTags: Frag = img(src := url.str, title := tit.str)

    override def toPlainScalaTags: Frag = url.str
  }


  /**
    * we make it invariant tha plain cannot be empty
    */
  case class Plain(unicode: Unicode) extends Text {
    assert(!unicode.isEmpty)
    override def size: Int = unicode.size

    override def toScalaTags: Frag = unicode.str
    override def toPlainScalaTags: Frag = toScalaTags

    private[model] override def serialize(buffer: UnicodeWriter): Unit = {
      buffer.put(unicode)
    }

    override def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean = {
      unicode.containsLowerCase(p)
    }

    override def after(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = unicode.after(i).map(u => Atom.PlainGrapheme(myCursor, myIndex + u._1, u._1, u._2, this))
    override def before(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = unicode.before(i).map(u => Atom.PlainGrapheme(myCursor, myIndex + u._1, u._1, u._2, this))

  }
}
