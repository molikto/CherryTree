package model.data

import boopickle._
import model.range.IntRange
import model.cursor

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
/**
  * we currently expect all our rich object is normalized??
  */
case class Rich(text: Seq[Text]) {


  lazy val size: Int = Text.size(text)

  def befores(a: Int): Iterator[Atom] = Text.before(Seq.empty, 0, a, text)
  def afters(a: Int): Iterator[Atom] = Text.after(Seq.empty, 0, a, text)

  def beginning: Atom = if (isEmpty) throw new IllegalArgumentException("Empty") else after(0)

  def end: Atom = if (isEmpty) throw new IllegalArgumentException("Empty") else before(size)

  def before(a: IntRange): Atom = before(a.start)
  def after(a: IntRange): Atom = after(a.until)
  // atomic consists: single unicode grapheme, single control point, atomic views
  def before(aaa: Int): Atom = { val i = befores(aaa); if (i.hasNext) i.next() else beginning }
  def after(bbb: Int): Atom =  { val i = afters(bbb); if (i.hasNext) i.next() else end }

  def rangeBefore(a: IntRange): IntRange = if (a.start == 0) a else before(a.start).range
  def rangeAfter(a: IntRange): IntRange = if(a.until == size) a else after(a.until).range
  // atomic consists: single unicode grapheme, single control point, atomic views
  def rangeBefore(aaa: Int): IntRange = if (aaa == 0) rangeBeginning else before(aaa).range
  def rangeAfter(bbb: Int): IntRange = if (bbb == size) rangeEnd else after(bbb).range

  def rangeBeginning: IntRange = if (size == 0) IntRange(0, 0) else beginning.range
  def rangeEnd: IntRange = if (size == 0) IntRange(0, 0) else end.range


  def between(a: Int, b: Int): Iterator[Atom] = between(IntRange(a, b))
  def between(r: IntRange): Iterator[Atom] = afters(r.start).takeWhile(_.range.until <= r.until)

  def singleSpecials(r: IntRange): Seq[Atom.Special[Any]] = {
    between(r).filter(a => {
      val singleSpecial = a.special && !r.contains(a.asInstanceOf[Atom.Special[Any]].another.range)
      singleSpecial
    }).map(_.asInstanceOf[Atom.Special[Any]]).toSeq
  }
  /**
    * a word is a:
    * continuous sequence of letter, digits or underscore
    * a continuous sequence of other non-blank characters
    * a atomic LaTeX or image or
    *
    * control characters is considered as space
    *
    * a WORD is:
    * a continuous sequence of word
    */
  /**
    * next word start
    */
  def moveRightWord(a: IntRange): Option[IntRange] = {
    val atom = before(a.until)
    util.head((if (atom.letterLike)
      afters(a.until).dropWhile(_.letterLike)
    else if (atom.charNonLetterLike)
      afters(a.until).dropWhile(_.charNonLetterLike)
    else
      afters(a.until)
      ).dropWhile(_.whitespace)).map(_.range)
  }

  /**
    * next WORD start
    */
  def moveRightWORD(a: IntRange): Option[IntRange] = {
    val atom = before(a.until)
    util.head((if (!atom.whitespace)
      afters(a.until).dropWhile(!_.whitespace)
    else
      afters(a.until)
      ).dropWhile(_.whitespace)).map(_.range)
  }

  /**
    * next word end
    */
 def moveRightWordEnd(a: IntRange): Option[IntRange] = {
     def rec(a: IntRange): Option[IntRange] = {
       val atom = after(a.until)
       if (atom.letterLike)
         util.last(afters(a.until).takeWhile(_.letterLike)).map(_.range)
       else if (atom.charNonLetterLike)
         util.last(afters(a.until).takeWhile(_.charNonLetterLike)).map(_.range)
       else
         util.last(afters(a.until).takeWhile(_.whitespace)).map(_.range).flatMap(rec)
     }
     rec(a)
 }

  /**
    * next WORD end
    */
  def moveRightWORDEnd(a: IntRange): Option[IntRange] = {
    def rec(a: IntRange): Option[IntRange] = {
      val atom = after(a.until)
      if (!atom.whitespace)
        util.last(afters(a.until).takeWhile(!_.whitespace)).map(_.range)
      else
        util.last(afters(a.until).takeWhile(_.whitespace)).map(_.range).flatMap(rec)
    }
    rec(a)
  }



  def moveLeftWord(a: Int): Option[IntRange] = {
    def rec(a: Int): Option[IntRange] = {
      val atom = before(a)
      if (atom.letterLike)
        util.last(befores(a).takeWhile(_.letterLike)).map(_.range)
      else if (atom.charNonLetterLike)
        util.last(befores(a).takeWhile(_.charNonLetterLike)).map(_.range)
      else
        util.last(befores(a).takeWhile(_.whitespace)).map(_.range).flatMap(a => rec(a.start))
    }
    rec(a)
  }
  /**
    * previous word start
    */
  def moveLeftWord(a: IntRange): Option[IntRange] = {
    moveLeftWord(a.start)
  }

  /**
    * previous WORD start
    */
  def moveLeftWORD(a: IntRange): Option[IntRange] = {
      def rec(a: IntRange): Option[IntRange] = {
        val atom = before(a.start)
        if (!atom.whitespace)
          util.last(befores(a.start).takeWhile(!_.whitespace)).map(_.range)
        else
          util.last(befores(a.start).takeWhile(_.whitespace)).map(_.range).flatMap(rec)
      }
      rec(a)
  }

  /**
    * previous word end
    */
  def moveLeftWordEnd(a: IntRange): Option[IntRange] = {
    val atom = after(a.start)
    util.head((if (atom.letterLike)
      befores(a.start).dropWhile(_.letterLike)
    else if (atom.charNonLetterLike)
      befores(a.start).dropWhile(_.charNonLetterLike)
    else
      befores(a.start)
      ).dropWhile(_.whitespace)).map(_.range)
  }

  /**
    * previous WORD end
    */
  def moveLeftWORDEnd(a: IntRange): Option[IntRange] = {
    val atom = before(a.start)
    util.head((if (!atom.whitespace)
      befores(a.start).dropWhile(!_.whitespace)
    else
      befores(a.start)
      ).dropWhile(_.whitespace)).map(_.range)
  }

  def extendToWORDOrWhitespace(a: IntRange): Option[IntRange] = {
    ???
  }

  def extendToWORDAndTailingWhitespace(a: IntRange): Option[IntRange] = {
    ???
  }

  def extendToWordOrWhitespace(a: IntRange): Option[IntRange] = {
    ???
  }

  def extendToWordAndTailingWhitespace(a: IntRange): Option[IntRange] = {
    ???
  }


  // LATER better implementation
  def subPlain(p: IntRange): Unicode = serialize().slice(p)

  def insideCoded(pos: Int): Boolean = {
    val bs = befores(pos)
    val as = afters(pos)
    if (!bs.hasNext || !as.hasNext) false
    else {
      (bs.next(), as.next()) match {
        case (_: Atom.CodedGrapheme, _: Atom.CodedGrapheme) => true
        case _ => false
      }
    }
  }


  def findCharAfter(start: IntRange, grapheme: Unicode, delimitationCodePoints: Map[SpecialChar, Unicode]): Option[Atom] = {
    afters(start.until).find(_.matches(grapheme, delimitationCodePoints))
  }

  def findCharBefore(start: IntRange, grapheme: Unicode, delimitationCodePoints: Map[SpecialChar, Unicode]): Option[Atom] = {
    befores(start.start).find(_.matches(grapheme, delimitationCodePoints))
  }



  def copyTextualRange(r: IntRange): Seq[Text] = {
    val ss = singleSpecials(r)
    val reverses = ss.map(_.another)
    Text.assemble((between(r).toSeq ++ reverses).sortBy(_.totalIndex))
  }





  def isEmpty: Boolean = text.isEmpty


  private[model] def serialize(): Unicode = {
    Text.serialize(text)
  }

  def isSubRich(range: IntRange): Option[IntRange] = {
    if (range.size == 0) return None
    def isValidStart(_1: Atom): Boolean = _1 match {
      case p: Atom.PlainGrapheme => true
      case s: Atom.Marked => true
      case s => s.delimitationStart
    }

    def isValidEnd(_1: Atom): Boolean = _1 match {
      case p: Atom.PlainGrapheme => true
      case s: Atom.Marked => true
      case s => s.delimitationEnd
    }
    val a = after(range.start)
    val b = before(range.until)
    if (isValidStart(a) && isValidEnd(b)) {
      if (a.nodeCursor == b.nodeCursor) {
        return Some(IntRange(a.range.start, b.range.until))
      } else if (a.nodeCursor.dropRight(1) == b.nodeCursor.dropRight(1)) {
        return Some(IntRange(a.range.start, b.range.until))
      }
    }
    None
  }
}

object Rich extends DataObject[Rich] {


  val empty: Rich = Rich(Seq.empty)

  private[model] def parse(unicode: Unicode): Rich = {
    val reader = new UnicodeReader(unicode)
    Rich(Text.parseAll(reader))
  }

  override val pickler: Pickler[Rich] = new Pickler[Rich] {
    override def pickle(obj: Rich)(implicit state: PickleState): Unit = Unicode.pickler.pickle(obj.serialize())
    override def unpickle(implicit state: UnpickleState): Rich = parse(Unicode.pickler.unpickle)
  }


  override def random(r: Random): Rich = Rich(randomWithDepth(r, 0))

  private def randomWithDepth(r: Random, depth: Int): Seq[Text] = {
    val childsAtDepth = depth match {
      case 0 => 5
      case 1 => 4
      case 2 => 4
      case 3 => 2
      case _ => 1
    }
    val addAtDepth = if (depth == 0) 3 else 0
    // we normalize this so that parsed result is the same with non-parsed
    val nonNormalized = (0 until (addAtDepth + r.nextInt(childsAtDepth))).map(_ => randomText(r, depth + 1))
    nonNormalized.foldRight(Seq.empty[Text]) { (m, seq) =>
      (m, seq.headOption) match {
        case (Text.Plain(c), Some(Text.Plain(j))) =>
          Text.Plain(c + j) +: seq.tail
        case _ => m +: seq
      }
    }
  }

  private def randomText(r: Random, depth: Int): Text = {
    r.nextInt(8) match {
      case 0 => Text.Strong(randomWithDepth(r, depth))
      case 1 => Text.Emphasis(randomWithDepth(r, depth))
      case 3 => Text.Code(Unicode.random(r))
      case 4 => Text.StrikeThrough(randomWithDepth(r, depth))
      case 5 => Text.LaTeX(Unicode.random(r))
      case 6 => Text.Image(
        Unicode.random(r),
        Unicode.random(r))
      case 7 => Text.Link(
        randomWithDepth(r, depth),
        Unicode.random(r),
        Unicode.random(r))
      case _ => Text.Plain(Unicode.random(r))
    }
  }


}
