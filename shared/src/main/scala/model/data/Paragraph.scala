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
  nodePosition: cursor.Node,
  nodeStart: Int,
  text: Text,
  ty: InfoType,
  charPosition: Int = -1, // only valid if not Special
  specialChar: SpecialChar = null, // only valid if type == Special, the special char, or type == Attribute, the attribute name
) {
  def isSpecialChar(a: SpecialChar): Boolean = ty == InfoType.Special && specialChar == a
  def isStart: Boolean = ty == InfoType.Special && SpecialChar.starts.contains(specialChar)
  def isEnd: Boolean = ty == InfoType.Special && SpecialChar.ends.contains(specialChar)
  def isAttributeTag: Boolean = SpecialChar.attributes.contains(specialChar)
  def extendedGraphemeRange(): IntRange = {
    if (ty == InfoType.Plain) {
      text.asInstanceOf[Text.Plain].unicode.extendedGraphemeRange(charPosition).moveBy(nodeStart)
    } else if (ty == InfoType.Coded) {
      text.asInstanceOf[Text.Coded].unicode.extendedGraphemeRange(charPosition).moveBy(nodeStart + 1)
    } else {
      throw new IllegalStateException("Invalid xml structure")
    }
  }
}
/**
  * we currently expect all our paragraph object is normalized
  */
case class Paragraph(text: Seq[Text]) {

  def moveLeftAtomic(aaa: Int): IntRange = {
    val (pos, info) = infoSkipLeftAttributes(aaa - 1)
    // three cases of selection, atomic, special char, unicode
    if (info.text.isAtomicViewed) {
      val atomicStart = info.nodeStart
      IntRange(atomicStart, atomicStart + info.text.size)
    } else if (info.ty == InfoType.Special) {
      IntRange(pos)
    } else {
      info.extendedGraphemeRange()
    }
  }

  def moveRightAtomic(bbb: Int): IntRange = {
    val (pos, info) = infoSkipRightAttributes(bbb + 1)
    // three cases of selection, atomic, special char, unicode
    if (info.text.isAtomicViewed) {
      val atomicStart = info.nodeStart
      IntRange(atomicStart, atomicStart + info.text.size)
    } else if (info.ty == InfoType.Special) {
      IntRange(pos)
    } else {
      info.extendedGraphemeRange()
    }
  }

  def infoSkipLeftAttributes(pos: Int): (Int, Info) = {
    var i = pos
    while (i >= 0) {
      val f = info(i)
      if (f.ty == InfoType.AttributeUnicode) {
        i -= f.charPosition + 1
      } else if (f.isAttributeTag) {
        i -= 1
      } else {
        return (i, f)
      }
    }
    throw new IllegalStateException("Invalid xml structure")
  }

  def infoSkipRightAttributes(pos: Int): (Int, Info) = {
    var i = pos
    while (true) {
      val f = info(i)
      if (f.ty == InfoType.AttributeUnicode || f.isAttributeTag) {
        i = f.nodeStart + f.text.size - 1 // the last character
      } else {
        return (i, f)
      }
    }
    throw new IllegalStateException("Invalid xml structure")
  }


  def isEmpty: Boolean = {
    for (t <- text) {
      if (!t.isEmpty) return false
    }
    true
  }


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

  def info(a: Int): Info = Text.info(Seq.empty[Int], 0, text, a).right.get

  def defaultNormalMode(): mode.Content = mode.Content.Normal(text.headOption match {
    case Some(a: Text.AtomicViewed) => IntRange(0, a.size)
    case Some(a: Text.Formatted) => IntRange(0, 1)
    case Some(a: Text.Coded) => IntRange(0, 1)
    case Some(a: Text.Plain) => a.unicode.extendedGraphemeRange(0) // because plain cannot be empty
    case None => IntRange(0, 0)
  })
}

object Paragraph extends DataObject[Paragraph] {

  val empty: Paragraph = Paragraph(Seq.empty)

  private[model] def parse(unicode: Unicode): Paragraph = {
    val reader = new UnicodeReader(unicode)
    Paragraph(Text.parseAll(reader))
  }

  override val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = Unicode.pickler.pickle(obj.serialize())
    override def unpickle(implicit state: UnpickleState): Paragraph = parse(Unicode.pickler.unpickle)
  }


  override def random(r: Random): Paragraph = Paragraph(randomWithDepth(r, 0))

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
