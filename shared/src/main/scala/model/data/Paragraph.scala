package model.data

import boopickle._
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * we currently expect all our paragraph object is normalized
  */
object Paragraph extends DataObject[Paragraph] {

  def apply(text: Text*): Paragraph = text

  // TODO fix all these functions
  def randomParagraphInsertionPoint(d: Paragraph, r: Random): Int = {
    0
  }

  private[model] def randomTitleOrLink(d: Paragraph, r: Random): Option[(IntRange, IntRange)] = {
    None
  }

  private[model] def randomFormatted(d: Paragraph, r: Random): Option[IntRange] = {
    None
  }

  private[model] def randomCoded(d: Paragraph, r: Random): Option[IntRange] = {
    None
  }

  private[model] def randomSubparagraph(d: Paragraph, r: Random): IntRange = {
    IntRange(0, d.size - 1)
  }

  private[model] def serialize(content: Paragraph): Unicode = {
    val buffer = new UnicodeWriter()
    content.foreach(_.serialize(buffer))
    buffer.toUnicode
  }
  private[model] def parse(unicode: Unicode): Paragraph = {
    val reader = new UnicodeReader(unicode)
    parse(reader, SpecialChar.NotSpecial)
  }

  private[model] def parse(reader: UnicodeReader, until: SpecialChar.Type): Seq[Text] = {
    val buffer = new ArrayBuffer[Text]()
    while (!reader.isEmpty && !reader.eatOrFalse(until)) {
      buffer += Text.parse(reader)
    }
    buffer.toVector
  }

  def size(paragraph: Paragraph): Int = paragraph.map(_.size).sum

  override val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = Unicode.pickler.pickle(serialize(obj))
    override def unpickle(implicit state: UnpickleState): Paragraph = parse(Unicode.pickler.unpickle)
  }


  override def random(r: Random): Paragraph = randomWithDepth(r, 0)

  def randomWithDepth(r: Random, depth: Int): Paragraph = {
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
    r.nextInt(5) match {
      case 0 => Text.Strong(randomWithDepth(r, depth))
      case 1 => Text.Emphasis(randomWithDepth(r, depth))
      case 2 => Text.Plain(Unicode.random(r))
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
    }
  }
}
