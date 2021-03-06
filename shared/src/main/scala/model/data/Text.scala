package model.data

import java.util.UUID

import model.cursor
import model.data.SpecialChar.{Delimitation, DelimitationType}
import model.range.IntRange
import settings.SpecialKeySettings

import scala.collection.mutable.ArrayBuffer
import scalatags.Text.all._
import search.{Search, SearchOccurrence}


abstract sealed class Text {
  def mapBy(map: Map[UUID, UUID]): Text

  def toScalaTags(safe: Boolean): Frag
  def toPlainScalaTags: Frag

  def apply(cur: model.cursor.Node): Text = if (cur.isEmpty) this else throw new IllegalArgumentException("not possible")
  def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean = false
  def quickSearchHash(p: Unicode, deli: SpecialKeySettings): Boolean = false

  def isAtomic: Boolean = this.isInstanceOf[Text.Atomic]
  def isCoded: Boolean = this.isInstanceOf[Text.Coded]
  def asCoded: Text.Coded = this.asInstanceOf[Text.Coded]
  def isCodedNonAtomic: Boolean = this.isInstanceOf[Text.Coded] && asCoded.delimitation.codedNonAtomic
  def isCodedAtomic: Boolean = this.isInstanceOf[Text.Coded] && asCoded.delimitation.codedAtomic
  def isPlain: Boolean = this.isInstanceOf[Text.Plain]
  def isDelimited: Boolean = this.isInstanceOf[Text.Delimited]
  def asDelimited: Text.Delimited = this.asInstanceOf[Text.Delimited]
  def asPlain: Text.Plain = this.asInstanceOf[Text.Plain]

  private[data] def serialize(buffer: EncodedSeqWriter)
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
  def tags(text: Seq[Text], a: ArrayBuffer[HashTag]): Unit = {
    text.foreach {
      case h: HashTag => if (h.content.nonEmpty) a.append(h)
      case f: Formatted => tags(f.content, a)
      case _ =>
    }
  }


  private[data] def toScalaTags(a: Seq[Text], safe: Boolean): Frag = a.map(_.toScalaTags(safe)): Frag
  private[data] def toPlainScalaTags(a: Seq[Text]): Frag = a.map(_.toPlainScalaTags): Frag
  def toHtml(a: Seq[Text]): String = toScalaTags(a, false).render
  def toPlain(a: Seq[Text]): String = toPlainScalaTags(a).render

  def quickSearch(text: Seq[Text], p: Unicode, deli: SpecialKeySettings): Boolean = {
    text.exists(_.quickSearch(p, deli))
  }

  def quickSearchHash(text: Seq[Text], p: Unicode, deli: SpecialKeySettings): Boolean = {
    text.exists(_.quickSearchHash(p, deli))
  }


  private[model] def serialize(text: Seq[Text]): EncodedSeq = {
    val buffer = new EncodedSeqWriter()
    text.foreach(_.serialize(buffer))
    buffer.toEncodedSeq
  }


  def normalize(atoms: Seq[Text]): Seq[Text] = {
    Text.parseAll(new EncodedSeqReader(Text.serialize(atoms)))
  }

  def assemble(atoms: Seq[Atom]): Seq[Text] = {
    val buffer = new EncodedSeqWriter()
    atoms.foreach(_.serialize(buffer))
    Text.parseAll(new EncodedSeqReader(buffer.toEncodedSeq))
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

  sealed trait Atomic extends Delimited {
    final override def after(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = {
      if (i < size) Iterator.single(Atom.Marked(myCursor, myIndex, this))
      else Iterator.empty
    }

    final override def before(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = {
      if (i == size)
        Iterator.single(Atom.Marked(myCursor, myIndex, this))
      else Iterator.empty
    }

    override def mapBy(map: Map[UUID, UUID]): Text = this
  }

  def size(content: Seq[Text]): Int = content.map(_.size).sum

  private[model] def parse(reader: EncodedSeqReader): Text = {
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
        case HashTagStart =>
          HashTag(parseAll(reader, HashTagEnd))
        case SupStart =>
          Sup(parseAll(reader, SupEnd))
        case SubStart =>
          Sub(parseAll(reader, SubEnd))
        case SpanClassStart =>
          SpanClass(parseAll(reader, ClassAttribute), reader.eatUntilAndDrop(SpanClassEnd))
        case UnderlineStart =>
          Underline(parseAll(reader, UnderlineEnd))
        case HashDefStart =>
          HashDef(parseAll(reader, HashDefEnd))
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
          throw new EncodedSeqParseException(s"Expecting a non-special char or a special start char, but found $kk, reader:\n${reader.debugString}")
      }
      case None =>
        Plain(reader.eatUntilSpecialChar())
    }
  }

  private[model] def parseAll(reader: EncodedSeqReader): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty) {
      buffer += Text.parse(reader)
    }
    buffer.toSeq
  }

  private[model] def parseAll(reader: EncodedSeqReader, until: SpecialChar): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty && !reader.eatOrFalse(until)) {
      buffer += Text.parse(reader)
    }
    buffer
  }


  sealed trait Delimited extends Text {
    def contentSize: Int
    private[model] def serializeContent(buffer: EncodedSeqWriter): Unit
    private[model] def serializeAttributes(buffer: EncodedSeqWriter): Unit  = {
      attributes.foreach(a => {
        buffer.put(a)
        buffer.put(attribute(a))
      })
    }

    final private[model] override def serialize(buffer: EncodedSeqWriter): Unit = {
      buffer.put(delimitation.start)
      serializeContent(buffer)
      serializeAttributes(buffer)
      buffer.put(delimitation.end)
    }

    def delimitation: SpecialChar.Delimitation

    def attributeValues: Seq[Unicode] = attributes.map(attribute)
    def attributes: Seq[SpecialChar] = delimitation.attributes
    def attribute(i: SpecialChar): Unicode = throw new IllegalArgumentException("not possible")
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
      deli.get(delimitation.start).contains(p) || deli.get(delimitation.end).contains(p)
  }

  sealed trait Formatted extends DelimitedT[Seq[Text]] {

    override def mapBy(map: Map[UUID, UUID]): Text = copy(content.map(_.mapBy(map)))

    def copy(cs: Seq[Text]): Formatted


    def content: Seq[Text]
    def delimitation: SpecialChar.Delimitation
    lazy val contentSize: Int = Text.size(content)
    override def apply(cur: model.cursor.Node): Text = if (cur.isEmpty) this else content(cur.head)(cur.tail)

    override def toPlainScalaTags: Frag = Text.toPlainScalaTags(content)

    override def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean =
      super.quickSearch(p, deli) ||
      Text.quickSearch(content, p, deli)



    override def quickSearchHash(p: Unicode, deli: SpecialKeySettings): Boolean =
        Text.quickSearchHash(content, p, deli)


    override private[model] def serializeContent(buffer: EncodedSeqWriter): Unit = {
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

    override def toScalaTags(safe: Boolean): Frag = em(Text.toScalaTags(content, safe))

    override def copy(cs: Seq[Text]): Formatted = Emphasis(content)
  }
  case class Strong(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Strong
    override def toScalaTags(safe: Boolean): Frag = strong(Text.toScalaTags(content, safe))

    override def copy(cs: Seq[Text]): Formatted = Strong(content)
  }

  case class StrikeThrough(override val content: Seq[Text]) extends Formatted {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.StrikeThrough
    override def toScalaTags(safe: Boolean): Frag = del(Text.toScalaTags(content, safe))

    override def copy(cs: Seq[Text]): Formatted = StrikeThrough(content)
  }
  case class Link(content: Seq[Text], url: Unicode, tit: Unicode = Unicode.empty) extends Formatted {
    def isNodeRef: Boolean = url.str.startsWith(Node.NodeRefScheme)
    override def toScalaTags(safe: Boolean): Frag = a(Text.toScalaTags(content, safe) : Frag, href := url.str)
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content), s" (${url.str})": Frag)

    override def delimitation: SpecialChar.Delimitation = SpecialChar.Link
    override def attribute(i: SpecialChar): Unicode =
      if (i == UrlAttribute) url
      else if (i == TitleAttribute) tit
      else throw new IllegalArgumentException("Not here")

    override def copy(cs: Seq[Text]): Formatted = StrikeThrough(content)

    override def mapBy(map: Map[UUID, UUID]): Text = Link(content.map(_.mapBy(map)), Node.matchNodeRef(url.str) match {
      case Some(uuid) => Unicode(Node.nodRef(map.getOrElse(uuid, uuid)))
      case None => url
    }, tit)
  }

  case class HashTag(content: Seq[Text]) extends Formatted {
    override def delimitation: Delimitation = SpecialChar.HashTag

    // LATER what should this be?
    override def toScalaTags(safe: Boolean): Frag = if (safe) Text.toScalaTags(content, safe) else a(Text.toScalaTags(content, safe))
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content))

    override def quickSearchHash(p: Unicode, deli: SpecialKeySettings): Boolean =
      content == Seq(Plain(p))

    override def copy(cs: Seq[Text]): Formatted = HashTag(content)
  }

  case class SpanClass(content: Seq[Text], clz: Unicode) extends Formatted {
    val clzs = clz.str.split(" ")
    def displayClass(predefined: Seq[String], str: String): String = if (clzs.exists(a => predefined.contains(a))) clz.str else str


    override def attributes: Seq[_root_.model.data.SpecialChar] = Seq(ClassAttribute)

    override def delimitation: Delimitation = SpecialChar.SpanClass
    override def toScalaTags(safe: Boolean): Frag = span(cls := clz.str, Text.toScalaTags(content, safe))
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content))
    override def copy(cs: Seq[Text]): Formatted = SpanClass(content, clz)

    override def attribute(i: SpecialChar): Unicode =
      if (i == ClassAttribute) clz
      else throw new IllegalArgumentException("Not here")
  }

  case class Sub(content: Seq[Text]) extends Formatted {
    override def delimitation: Delimitation = SpecialChar.Sub
    override def toScalaTags(safe: Boolean): Frag = sub(Text.toScalaTags(content, safe))
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content))
    override def copy(cs: Seq[Text]): Formatted = Sub(content)
  }

  case class Underline(content: Seq[Text]) extends Formatted {
    override def delimitation: Delimitation = SpecialChar.Underline
    override def toScalaTags(safe: Boolean): Frag = u(Text.toScalaTags(content, safe))
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content))
    override def copy(cs: Seq[Text]): Formatted = Underline(content)
  }

  case class Sup(content: Seq[Text]) extends Formatted {
    override def delimitation: Delimitation = SpecialChar.Sup
    override def toScalaTags(safe: Boolean): Frag = sup(Text.toScalaTags(content, safe))
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content))
    override def copy(cs: Seq[Text]): Formatted = Sup(content)
  }

  case class HashDef(content: Seq[Text]) extends Formatted {
    override def delimitation: Delimitation = SpecialChar.HashDef

    // LATER what should this be?
    override def toScalaTags(safe: Boolean): Frag = if (safe) Text.toScalaTags(content, safe) else a(Text.toScalaTags(content, safe))
    override def toPlainScalaTags: Frag = Seq(Text.toPlainScalaTags(content))

    override def quickSearchHash(p: Unicode, deli: SpecialKeySettings): Boolean =
      content == Seq(Plain(p))

    override def copy(cs: Seq[Text]): Formatted = HashDef(content)
  }

  sealed trait Coded extends DelimitedT[Unicode] {
    def content: Unicode
    def delimitation: SpecialChar.Delimitation
    override def contentSize: Int = content.size



    //override def apply(cur: model.cursor.Node): Text = if (cur.isEmpty) this else if (cur == Se

    override def mapBy(map: Map[UUID, UUID]): Text = this

    override private[model] def serializeContent(buffer: EncodedSeqWriter): Unit = {
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

    override def toScalaTags(safe: Boolean): Frag = code(content.str)
    override def toPlainScalaTags: Frag = raw(content.str)
  }
  case class LaTeX(content: Unicode) extends Coded with Atomic {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.LaTeX

    override def toScalaTags(safe: Boolean): Frag = span(attr("data-latex") := content.str)

    override def toPlainScalaTags: Frag = raw(content.str)
  }

  case class HTML(content: Unicode) extends Coded with Atomic {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.HTML
    override def toScalaTags(safe: Boolean): Frag = if (safe) "" else raw(content.str)

    override def toPlainScalaTags: Frag = toScalaTags(false)
  }


  sealed trait DelimitedEmpty extends DelimitedT[Unit] {
    override def content: Unit = Unit
    override def contentSize: Int = 0
    override private[model] def serializeContent(buffer: EncodedSeqWriter): Unit = {}
  }


  case class Image(url: Unicode, tit: Unicode = Unicode.empty) extends DelimitedEmpty with Atomic {
    override def delimitation: SpecialChar.Delimitation = SpecialChar.Image
    override def attribute(i: SpecialChar): Unicode = if (i == UrlAttribute) url else if (i == TitleAttribute) tit else throw new IllegalArgumentException("Not here")

    override def toScalaTags(safe: Boolean): Frag = img(src := url.str, title := tit.str)

    override def toPlainScalaTags: Frag = raw(url.str)

  }


  /**
    * we make it invariant tha plain cannot be empty
    */
  case class Plain(unicode: Unicode) extends Text {
    assert(!unicode.isEmpty)
    override def size: Int = unicode.size

    override def toScalaTags(safe: Boolean): Frag = unicode.str
    override def toPlainScalaTags: Frag = raw(unicode.str)

    private[model] override def serialize(buffer: EncodedSeqWriter): Unit = {
      buffer.put(unicode)
    }

    override def quickSearch(p: Unicode, deli: SpecialKeySettings): Boolean = {
      unicode.containsLowerCase(p)
    }

    override def after(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = unicode.after(i).map(u => Atom.PlainGrapheme(myCursor, myIndex + u._1, u._1, u._2, this))
    override def before(myCursor: model.cursor.Node, myIndex: Int, i: Int): Iterator[Atom] = unicode.before(i).map(u => Atom.PlainGrapheme(myCursor, myIndex + u._1, u._1, u._2, this))

    override def mapBy(map: Map[UUID, UUID]): Text = this

  }

  def search(texts: Seq[Text], a: Search, startPos0: Int): Option[IntRange] = {
    val sizeSum = texts.map(_.size).sum
    val startPos = startPos0 min sizeSum
    if (a.direction >= 0) {
      var before = 0
      for (t <- texts) {
        if (before + t.size >= startPos) {
          t match {
            case f: Formatted =>
              val res = search(f.content, a, startPos - before - 1).map(_.moveBy(1 + before))
              if (res.isDefined) {
                return res
              }
            case a: Atomic =>
            case Plain(u) =>
              val index = u.indexOfLower(a.lowerTerm, startPos - before)
              if (index >= 0) {
                return Some(IntRange.len(before + index, a.lowerTerm.size))
              }
            case c: Coded =>
              val index = c.content.indexOfLower(a.lowerTerm, startPos - before - 1)
              if (index >= 0) {
                return Some(IntRange.len(before + index + 1, a.lowerTerm.size))
              }
          }
        } else if (before >= startPos) {
          return None
        }
        before += t.size
      }
    } else {
      var before = sizeSum
      for (t <- texts.reverseIterator) {
        before -= t.size
        if (before <= startPos) {
          t match {
            case f: Formatted =>
              val res = search(f.content, a, startPos - before - 1).map(_.moveBy(1 + before))
              if (res.isDefined) {
                return res
              }
            case a: Atomic =>
            case Plain(u) =>
              val index = u.lastIndexOfLower(a.lowerTerm, startPos - before)
              if (index >= 0) {
                return Some(IntRange.len(before + index, a.lowerTerm.size))
              }
            case c: Coded =>
              val index = c.content.lastIndexOfLower(a.lowerTerm, startPos - before - 1)
              if (index >= 0) {
                return Some(IntRange.len(before + index + 1, a.lowerTerm.size))
              }
          }
        } else if (before + t.size < startPos) {
          return None
        }
      }
    }
    None
  }

}
