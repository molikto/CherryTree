package model.data

import boopickle._
import model.range.IntRange
import model.{cursor, mode}

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
  def matchesChar(char: Int, delimitationCodePoints:  Map[SpecialChar.Delimitation, Int]): Boolean = {
    if (ty == InfoType.Special) {
      delimitationCodePoints.get(text.asInstanceOf[Text.Delimited[Any]].delimitation).contains(char)
    } else {
      this.char == char
    }
  }

  def isSpecialChar(a: SpecialChar): Boolean = ty == InfoType.Special && specialChar == a
  def isStart: Boolean = ty == InfoType.Special && SpecialChar.starts.contains(specialChar)
  def isEnd: Boolean = ty == InfoType.Special && SpecialChar.ends.contains(specialChar)
  def isAttributeTag: Boolean = SpecialChar.attributes.contains(specialChar)
  def positionInUnicode: Int = {
    text match {
      case p : Text.Plain if ty == InfoType.Plain => positionInParagraph - nodeStart
      case c : Text.Coded if ty == InfoType.Coded => positionInParagraph - nodeStart - 1
      case _ => throw new NotImplementedError("Not implemented yet!!")
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
  * we currently expect all our rich object is normalized???
  */
case class Rich(text: Seq[Text]) {



  def moveLeftAtomic(a: IntRange): IntRange = if (a.start == 0) a else moveLeftAtomic(a.start)
  def moveRightAtomic(a: IntRange): IntRange = if (a.until == size) a else moveRightAtomic(a.until - 1)

  def moveLeftAtomic(aaa: Int): IntRange = infoSkipLeftAttributes(aaa - 1).atomicRange

  def findRightCharAtomic(start: IntRange, char: Int, delimitationCodePoints:  Map[SpecialChar.Delimitation, Int]): Option[IntRange] = {
    var range = start
    while (range.until < size) {
      val info = infoSkipRightAttributes(range.until)
      range = info.atomicRange
      if (info.matchesChar(char, delimitationCodePoints)) {
        return Some(range)
      }
    }
    None
  }

  def findLeftCharAtomic(start: IntRange, char: Int, delimitationCodePoints: Map[SpecialChar.Delimitation, Int]): Option[IntRange] = {
    var range = start
    while (range.start > 0) {
      val info = infoSkipLeftAttributes(range.start - 1)
      range = info.atomicRange
      if (info.matchesChar(char, delimitationCodePoints)) {
        return Some(range)
      }
    }
    None
  }

  def moveRightAtomic(bbb: Int): IntRange = infoSkipRightAttributes(bbb + 1).atomicRange



  def infoSkipLeftAttributes(pos: Int): Info = {
    var i = pos
    while (i >= 0) {
      val f = info(i)
      if (f.ty == InfoType.AttributeUnicode) {
        i -= f.positionInUnicode + 1
      } else if (f.isAttributeTag) {
        i -= 1
      } else {
        return f
      }
    }
    throw new IllegalStateException("Invalid xml structure")
  }

  def infoSkipRightAttributes(pos: Int): Info = {
    var i = pos
    while (true) {
      val f = info(i)
      if (f.ty == InfoType.AttributeUnicode || f.isAttributeTag) {
        i = f.nodeStart + f.text.size - 1 // the last character
      } else {
        return f
      }
    }
    throw new IllegalStateException("Invalid xml structure")
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
