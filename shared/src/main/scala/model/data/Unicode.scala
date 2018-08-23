package model.data

import java.io.IOException
import java.util.Locale
import java.util.stream.IntStream

import model._
import model.range.IntRange
import util.GraphemeSplitter

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class UnicodeParseException(msg: String) extends IllegalStateException(msg) {
}


private[model] class UnicodeWriter {

  private val sb = new StringBuilder()

  private var size = 0

  def put(a: SpecialChar): Unit = {
    sb.append(Unicode(a))
    size += 1
  }

  def put(url: Unicode): Unit = {
    sb.append(url.str)
    size += url.size
  }

  def toUnicode: Unicode = {
    Unicode(sb.toString(), size, false)
  }
}

private[model] class UnicodeReader(a: Unicode) {
  private val str = a.str
  private var start = 0


  override def toString: String = str

  def isEmpty: Boolean = start >= str.length

  private def isSpecialCodePoint(c: Int) = SpecialChar.special(c)

  def eatOrNotSpecial(): Option[SpecialChar] = {
    val c = str.codePointAt(start)
    if (isSpecialCodePoint(c)) {
      start = str.offsetByCodePoints(start, 1)
      Some(SpecialChar(c - SpecialCharStart))
    } else {
      None
    }
  }

  def eat(a: SpecialChar) : Unit = {
    if (!eatOrFalse(a)) {
      throw new UnicodeParseException("!!")
    }
  }

  def eatOrFalse(a: SpecialChar): Boolean = {
    if (str.codePointAt(start) == SpecialCharStart + a.id) {
      start = str.offsetByCodePoints(start, 1)
      true
    } else {
      false
    }
  }

  def eatUntilSpecialChar(): Unicode = {
    var index = start
    var codePointCount = 0
    var isASCII = true
    while (index < str.length && !isSpecialCodePoint({
      val codepoint = str.codePointAt(index)
      if (util.isAscii(codepoint)) {
      } else {
        isASCII = false
      }
      codepoint
    })) {
      index = str.offsetByCodePoints(index, 1)
      codePointCount += 1
    }
    val ret = str.substring(start, index)
    start = index
    Unicode(ret, codePointCount, isASCII)
  }

  def eatUntilAndDrop(b: SpecialChar): Unicode = {
    val a = eatUntilSpecialChar()
    if (!eatOrFalse(b)) {
      throw new UnicodeParseException(s"Expecting $b")
    }
    a
  }
}
object Unicode extends DataObject[Unicode] {
  override val pickler: boopickle.Pickler[Unicode] = new boopickle.Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.str)
    override def unpickle(implicit state: UnpickleState): Unicode = Unicode(state.dec.readString)
  }

  def apply(a: Seq[String]): Unicode = {
    if (a.size == 1 ) {
      Unicode(a.head)
    } else {
      Unicode(a.mkString(""))
    }
  }

  def apply(a: String, size: Int, isAscii: Boolean = false): Unicode = {
    val u = Unicode(a)
    u.size0 = if (isAscii) -2 else size
    u
  }

  def apply(a: SpecialChar): Unicode = {
    Unicode(new String(Character.toChars(SpecialCharStart + a.id)), 1, false)
  }

  def specials(a: Seq[SpecialChar]): Unicode = {
    Unicode(a.map(apply).mkString, a.size, false)
  }

  def apply(a: Int): Unicode = {
    Unicode(new String(Character.toChars(a)), 1, false)
  }

  override def random(r: Random): Unicode = Unicode(r.nextLong().toString)
  val empty = Unicode("", 0, true)
}

case class Unicode(var str: String) extends Seq[Int] {

  // LATER faster??
  def containsLowerCase(p: Unicode): Boolean = str.toLowerCase().contains(p.str)

  def isBlank: Boolean = {
    var i = 0
    while (i < str.length) {
      val c = str.charAt(i)
      if (c != ' ' && c != '\t' && c != '\n' && c != '\r') {
        return false
      }
      i += 1
    }
    true
  }

  def guessProp: Unicode = {
    if (size0 != -2) {
      if (util.isAscii(str)) {
        size0 = -2
      }
    }
    this
  }


  override def length: Int = size

  override def size: Int = {
    if (size0 == -2) {
      return str.length
    } else if (size0 < 0) {
      size0 = str.codePointCount(0, str.length)
    }
    size0
  }

  private var lastCharIndex = -1
  private var lastCodePointIndex = -1
  private var size0 = -1

  override def apply(i: Int): Int = str.codePointAt(toStringPosition(i))

  override def head: Int = apply(0)

  def fromStringPosition(i: Int): Int = {
    if (noSurrogatePairBeforeAndAtCodePointIndex(i)) {
      i
    } else {
      val index = if (lastCodePointIndex == -1 || i < lastCharIndex) {
        str.codePointCount(0, i)
      } else {
        str.codePointCount(lastCharIndex, i) + lastCodePointIndex
      }
      lastCharIndex = i
      lastCodePointIndex = index
      index
    }
  }

  def toStringPosition(i: Int): Int = {
    if (noSurrogatePairBeforeAndAtCodePointIndex(i)) {
      i
    } else {
      val index = if (lastCodePointIndex == -1 || i * 2 <= lastCodePointIndex) {
        str.offsetByCodePoints(0, i)
      } else {
        str.offsetByCodePoints(lastCharIndex, i - lastCodePointIndex)
      }
      lastCharIndex = index
      lastCodePointIndex = i
      index
    }
  }


  override def iterator: Iterator[Int] = new Iterator[Int] {
    var i = 0
    override def hasNext: Boolean = i != str.length

    override def next(): Int = {
      val r = str.codePointAt(i)
      i = str.offsetByCodePoints(i, 1)
      r
    }
  }

  private def noSurrogatePairBeforeAndAtCodePointIndex(pos: Int): Boolean = {
    if (size0 == -2) {
      true
    } else {
      if (size0 != -1) {
        if (size0 == str.length) {
          return true
        }
      }
      lastCharIndex == lastCodePointIndex && pos < lastCodePointIndex
    }
  }

  def before(b: Int): Iterator[(Int, Unicode)] =
    if (size0 == -2) {
      new Iterator[(Int, Unicode)] {
        if (b > Unicode.this.size) throw new IllegalArgumentException(s"Not possible $b")

        var i = b

        override def hasNext: Boolean = i > 0

        override def next(): (Int, Unicode) = {
          if (i - 2 >= 0 && str.charAt(i - 2) == 0x0d && str.charAt(i - 1) == 0x0a) {
            i -= 2
            (i, Unicode(str.substring(i, i + 2)))
          } else {
            i -= 1
            (i, Unicode(str.substring(i, i + 1)))
          }
        }
      }

    } else {
      new Iterator[(Int, Unicode)] {
        if (b > Unicode.this.size) throw new IllegalArgumentException(s"Not possible $b")

        private val g = graphemes

        private val k = new ArrayBuffer[(Int, Unicode)]()

        {
          var i = 0
          while (i < b) {
            val gg = g.next()
            k.append(gg)
            i += gg._2.size
          }
        }

        private var j = k.size - 1

        override def hasNext: Boolean = j >= 0

        override def next(): (Int, Unicode) = {
          val r = k(j)
          j -= 1
          r
        }
      }
    }

  def after(b: Int): Iterator[(Int, Unicode)] = new Iterator[(Int, Unicode)] {

    // LATER implement https://fossies.org/linux/swift-swift/stdlib/public/core/StringCharacterView.swift
    // https://github.com/apple/swift/blob/a4230ab2ad37e37edc9ed86cd1510b7c016a769d/stdlib/public/core/StringGraphemeBreaking.swift#L202
    private var ii = b
    private var i = toStringPosition(b)
    override def hasNext: Boolean = i < str.length

    override def next(): (Int, Unicode) = {
      // from https://github.com/apple/swift/blob/a4230ab2ad37e37edc9ed86cd1510b7c016a769d/stdlib/public/core/StringGraphemeBreaking.swift
      val p = i
      if (size0 == -2) {
        if (i + 1 < str.length && str.charAt(i) == 0x0d && str.charAt(i + 1) == 0x0a) {
          i = i + 2
          val res = (ii, Unicode(str.substring(p, i)))
          ii += 2
          res
        } else {
          i += 1
          val res = (ii, Unicode(str.substring(p, i)))
          ii += 1
          res
        }
      } else {
        i = GraphemeSplitter.nextBreak(str, p)
        val sub = str.substring(p, i)
        val res = (ii, Unicode(sub))
        ii += str.substring(p, i).codePointCount(0, sub.length)
        res
      }
    }
  }

  private def toStrList: Seq[String] = {
    val sp = new ArrayBuffer[String]()
    var i = 0
    while (i < str.length) {
      val oi = i
      i = GraphemeSplitter.nextBreak(str, i)
      sp.append(str.substring(oi, i))
    }
    sp
  }

//  def diff(to: Unicode): Seq[operation.Unicode] = {
//    if (this.isBlank) {
//      return Seq(operation.Unicode.Insert(0, to))
//    }
//    val time = System.currentTimeMillis()
//    val diff = util.diff.Diff.create(toStrList, to.toStrList).diffs
//    var olen = 0
//    val ops = new ArrayBuffer[operation.Unicode]()
//    var i = 0
//    while (i < diff.size) {
//      val d = diff(i)
//      d.op match {
//        case OperationType.Insert =>
//          val j = i
//          while (i < diff.size && diff(i).op == OperationType.Insert) {
//            i += 1
//          }
//          val text = Unicode(diff.slice(j, i).flatMap(_.text))
//          ops.append(operation.Unicode.Insert(olen, text))
//          olen += text.size
//        case OperationType.Delete =>
//          val j = i
//          while (i < diff.size && diff(i).op == OperationType.Delete) {
//            i += 1
//          }
//          val text = Unicode(diff.slice(j, i).flatMap(_.text))
//          val size = text.size
//          ops.append(operation.Unicode.Delete(olen, olen + size))
//        case OperationType.Equals =>
//          val size = Unicode(d.text).size
//          olen += size
//          i += 1
//      }
//    }
//    ops
//  }

  def graphemes: Iterator[(Int, Unicode)] = after(0)

  def +(j: Unicode): Unicode = Unicode(str + j.str,
    if (size0 < 0 || j.size0 < 0) -1 else size + j.size, size0 == -2 && j.size0 == -2)


  override def toString: String = str


  override def isEmpty: Boolean = str.isEmpty
  def slice(r: IntRange): Unicode = {
    val start = toStringPosition(r.start)
    val end = toStringPosition(r.until)
    Unicode(str.substring(start, end), r.size, size0 == -2)
  }
  def insert(at: Int, u: Unicode): Unicode = {
    if (at < 0 || at > size) throw new IllegalArgumentException("Out of bound")
    val index = toStringPosition(at)
    Unicode(s"${str.substring(0, index)}${u.str}${str.substring(index)}", if (size0 < 0 || u.size0 < 0) -1 else size0 + u.size0, size0 == -2 && u.size0 == -2)
  }
  def delete(r: IntRange): Unicode = {
    if (r.size == 0) {
      this
    } else {
      val start = toStringPosition(r.start)
      val end = toStringPosition(r.until)
      Unicode(s"${str.substring(0, start)}${str.substring(end)}", if (size0 < 0) -1 else size0 - r.size, size0 == -2)
    }
  }

  def replace(r: IntRange, unicode: Unicode): Unicode = {
    val start = toStringPosition(r.start)
    val end = toStringPosition(r.until)
    Unicode(s"${str.substring(0, start)}${unicode.str}${str.substring(end)}", if (size0 < 0 || unicode.size0 < 0) -1 else size0 -r.size + unicode.size0, size0 == -2 && unicode.size == -2)
  }

  def surround(r: IntRange, left: Unicode, right: Unicode): Unicode = {
    val start = toStringPosition(r.start)
    val end = toStringPosition(r.until)
    val s = str.substring(start, end)
    Unicode(
      s"${str.substring(0, start)}${left.str}$s${right.str}${str.substring(end)}",
      if (size0 < 0 || left.size0 < 0 || right.size0 < 0) -1 else size0 + left.size0 + right.size0,
      size0 == -2 && left.size0 == -2 && right.size0 == -2
    )
  }

  def move(r: IntRange, at: Int): Unicode = {
    val s = slice(r)
    delete(r).insert(r.transformAfterDeleted(at).get, s)
  }

  def isDigit: Boolean = str.length == 1 && Character.isDigit(str.charAt(0))

  def asDigit: Int = str.toInt


  def containsSpace: Boolean = str.contains(" ")
}
