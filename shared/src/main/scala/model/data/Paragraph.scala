package model.data

import boopickle._
import model.cursor
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed class InfoType {
}
object InfoType {
  case class Special(a: SpecialChar) extends InfoType
  case object Plain extends InfoType
  case object Unicode extends InfoType
  case object Coded extends InfoType
}
case class Info(
  position: cursor.Node,
  text: Text,
  ty: InfoType
)
/**
  * we currently expect all our paragraph object is normalized
  */
case class Paragraph(text: Seq[Text]) {

  private[model] def serialize(): Unicode = {
    val buffer = new UnicodeWriter()
    text.foreach(_.serialize(buffer))
    buffer.toUnicode
  }

  lazy val size: Int = Text.size(text)

  def info(): Seq[Info] = {
    val buffer = new ArrayBuffer[Info]()
    text.zipWithIndex.foreach(a => a._1.info(buffer, Seq(a._2)))
    buffer.toVector
  }
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
        randomWithDepth(r, depth),
        Unicode.random(r),
        if (r.nextBoolean()) Some(Unicode.random(r)) else None)
      case 7 => Text.Link(
        randomWithDepth(r, depth),
        Unicode.random(r),
        if (r.nextBoolean()) Some(Unicode.random(r)) else None)
      case _ => Text.Plain(Unicode.random(r))
    }
  }


}
