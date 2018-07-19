package model.data

import boopickle._
import model.range.IntRange
import model.{cursor}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed class InfoType {
}
object InfoType {
  case object Special extends InfoType
  case object Plain extends InfoType
  case object AttributeUnicode extends InfoType
  case object Coded extends InfoType
}
case class Info(
  nodeCursor: cursor.Node,
  nodeStart: Int,
  text: Text,
  ty: InfoType,
  positionInParagraph: Int,
  specialChar: SpecialChar = null, // only valid if type == Special, the special char, or type == Attribute, the attribute name
  char: Int = 0 // only valid not Special
) {
  def matchesChar(grapheme: Unicode, delimitationCodePoints:  Map[SpecialChar, Int]): Boolean = {
    if (ty == InfoType.Special && grapheme.size == 1) {
      delimitationCodePoints.get(specialChar).contains(grapheme.codePoints.head)
    } else {
      extendedGrapheme == grapheme
    }
  }

  def isSpecialChar(a: SpecialChar): Boolean = ty == InfoType.Special && specialChar == a
  def isStart: Boolean = ty == InfoType.Special && SpecialChar.starts.contains(specialChar)
  def isEnd: Boolean = ty == InfoType.Special && SpecialChar.ends.contains(specialChar)
  def isEndOrAttributeTagOrContent: Boolean = isEnd || isAttributeTag || ty == InfoType.AttributeUnicode
  def isAttributeTag: Boolean = SpecialChar.attributes.contains(specialChar)
  def positionInUnicode: Int = {
    val p = text match {
      case p : Text.Plain if ty == InfoType.Plain => positionInParagraph - nodeStart
      case c : Text.Coded if ty == InfoType.Coded => positionInParagraph - nodeStart - 1
      case _ => throw new NotImplementedError("Not implemented yet!!")
    }
    assert(p >= 0)
    p
  }

  def extendedGrapheme: Unicode = {
    if (ty == InfoType.Plain) {
      text.asInstanceOf[Text.Plain].unicode.extendedGrapheme(positionInUnicode)
    } else if (ty == InfoType.Coded) {
      text.asInstanceOf[Text.Coded].content.extendedGrapheme(positionInUnicode)
    } else {
      throw new IllegalStateException("Invalid xml structure")
    }
  }

  def extendedGraphemeRange: IntRange = {
    if (ty == InfoType.Plain) {
      text.asInstanceOf[Text.Plain].unicode.extendedGraphemeRange(positionInUnicode).moveBy(nodeStart)
    } else if (ty == InfoType.Coded) {
      text.asInstanceOf[Text.Coded].content.extendedGraphemeRange(positionInUnicode).moveBy(nodeStart + 1)
    } else {
      throw new IllegalStateException("Invalid xml structure")
    }
  }

  def atomicRange: IntRange = {
    if (text.isAtomicViewed) {
      val atomicStart = nodeStart
      IntRange(atomicStart, atomicStart + text.size)
    } else if (ty == InfoType.Special) {
      IntRange(positionInParagraph)
    } else {
      extendedGraphemeRange
    }
  }
}
/**
  * we currently expect all our rich object is normalized??
  */
case class Rich(text: Seq[Text]) {


  def insertionInsideCoded(pos: Int): Boolean = {
    if (pos == size) false
    else {
      info(pos).text.isCoded
    }
  }

  def moveLeftAtomic(a: IntRange): IntRange = if (a.start == 0) a else moveLeftAtomic(a.start)
  def moveRightAtomic(a: IntRange): IntRange = if (a.until == size) a else moveRightAtomic(a.until - 1)
  // atomic consists: single unicode grapheme, single control point, atomic views
  def moveLeftAtomic(aaa: Int): IntRange = infoSkipLeftAttributes((aaa - 1) max 0).atomicRange
  def moveRightAtomic(bbb: Int): IntRange = infoSkipRightAttributes((bbb + 1) min (size - 1)).atomicRange

  def findRightCharAtomic(start: IntRange, grapheme: Unicode, delimitationCodePoints: Map[SpecialChar, Int]): Option[IntRange] = {
    var range = start
    while (range.until < size) {
      val info = infoSkipRightAttributes(range.until)
      val oldRange = range
      range = info.atomicRange
      assert(oldRange.until < range.until)
      if (info.matchesChar(grapheme, delimitationCodePoints)) {
        return Some(range)
      }
    }
    None
  }

  def findLeftCharAtomic(start: IntRange, grapheme: Unicode, delimitationCodePoints: Map[SpecialChar, Int]): Option[IntRange] = {
    var range = start
    while (range.start > 0) {
      val info = infoSkipLeftAttributes(range.start - 1)
      val oldRange = range
      range = info.atomicRange
      assert(range.start < oldRange.start)
      if (info.matchesChar(grapheme, delimitationCodePoints)) {
        return Some(range)
      }
    }
    None
  }




  def infoSkipLeftAttributes(pos: Int): Info = {
    val f = info(pos)
    if (f.ty == InfoType.AttributeUnicode || f.isAttributeTag) {
      info(f.nodeStart + f.text.asInstanceOf[Text.Delimited[Any]].contentSize)
    } else {
      f
    }
  }

  def infoSkipRightAttributes(pos: Int): Info = {
    val f = info(pos)
    if (f.ty == InfoType.AttributeUnicode || f.isAttributeTag) {
      info(f.nodeStart + f.text.size - 1)
    } else {
      f
    }
  }


  def isEmpty: Boolean = text.isEmpty


  private[model] def serialize(): Unicode = {
    val buffer = new UnicodeWriter()
    text.foreach(_.serialize(buffer))
    buffer.toUnicode
  }

  lazy val size: Int = Text.size(text)

  private [model] lazy val infos: Seq[Info] = {
    val buffer = new ArrayBuffer[Info]()
    Text.info(Seq.empty, 0, text, buffer)
    buffer.toVector
  }

  def info(a: IntRange): Seq[Info] = infos.slice(a.start, a.start + a.size)

  def infoAndSingleSpecials(a: IntRange): (Seq[Info], Seq[IntRange], Seq[IntRange]) = {
    val ifs = info(a)
    val soc = new ArrayBuffer[IntRange]()
    val roc = new ArrayBuffer[IntRange]()
    for (i <- ifs) {
      if (i.isStart) {
        val contentSize = i.text.asInstanceOf[Text.Delimited[Any]].contentSize
        val end = i.nodeStart + i.text.size - 1
        if (!a.contains(end)) {
          soc.append(IntRange(i.nodeStart))
          roc.append(IntRange(i.nodeStart + contentSize + 1, i.nodeStart + i.text.size))
        }
      } else if (i.isEnd) {
        val start = i.nodeStart
        if (!a.contains(start)) {
          val contentSize = i.text.asInstanceOf[Text.Delimited[Any]].contentSize
          soc.append(IntRange(i.nodeStart + contentSize + 1, i.nodeStart + i.text.size))
          roc.append(IntRange(i.nodeStart))
        }
      }
    }
    (ifs, soc, roc)
  }

  def info(a: Int): Info = Text.info(Seq.empty[Int], 0, text, a)

  def beginningAtomicRange(): IntRange = text.headOption match {
    case Some(a: Text.AtomicSelected) => IntRange(0, a.size)
    case Some(a: Text.Formatted) => IntRange(0, 1)
    case Some(a: Text.Coded) => IntRange(0, 1)
    case Some(a: Text.Plain) => a.unicode.extendedGraphemeRange(0) // because plain cannot be empty
    case None => IntRange(0, 0)
  }

  def endAtomicRange(): IntRange = text.lastOption match {
    case Some(a: Text.AtomicSelected) => IntRange(size - a.size, size)
    case Some(a: Text.Formatted) => IntRange(size - 1, size)
    case Some(a: Text.Coded) => IntRange(size - 1, size)
    case Some(a: Text.Plain) => a.unicode.extendedGraphemeRange(a.unicode.size - 1).moveBy(size - a.size) // because plain cannot be empty
    case None => IntRange(0, 0)
  }

  def isSubRich(range: IntRange): Boolean = {
    if (range.size == 0) return true
    def isValidStart(_1: Info): Boolean = _1.ty match {
      case InfoType.Plain => true
      case InfoType.Special => SpecialChar.starts.contains(_1.specialChar)
      case _ => false
    }

    def isValidEnd(_1: Info): Boolean = _1.ty match {
      case InfoType.Plain => true
      case InfoType.Special => SpecialChar.ends.contains(_1.specialChar)
      case _ => false
    }
    val a = info(range.start)
    val b = info(range.until - 1)
    if (isValidStart(a) && isValidEnd(b)) {
      if (a.nodeCursor == b.nodeCursor) {
        return true
      } else if (a.nodeCursor.dropRight(1) == b.nodeCursor.dropRight(1)) {
        return true
      }
    }
    false
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
          Text.Plain(c.join(j)) +: seq.tail
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
