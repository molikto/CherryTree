package model.operation

import model.data.SpecialChar
import model.{data, _}
import model.operation.Type.Type
import model.range.IntRange

import scala.util.Random


/**
  * it is not intended to create paragraph operations directly via
  *
  *
  * atomic = code, latex | text
  * paragraph = seq(atomic) | formated(paragraph))
  */
case class Paragraph(u: Seq[Unicode], override val ty: Type) extends Operation[data.Paragraph] {
  override def apply(d: data.Paragraph): data.Paragraph =
    data.Paragraph.parse(Unicode.apply(u, d.serialize()))
}

object Paragraph extends OperationObject[data.Paragraph, Paragraph] {

  override val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = {
      import state.enc._
      writeInt(obj.u.size)
      obj.u.foreach(a => Unicode.pickler.pickle(a))
      writeInt(obj.ty.id)
    }

    override def unpickle(implicit state: UnpickleState): Paragraph = {
      import state.dec._
      Paragraph((0 until readInt).map(_ => Unicode.pickler.unpickle), Type(readInt))
    }
  }

  /**
    * this generate random changes based on data, not invoking the practical operation handlers
    */
  override def random(d: data.Paragraph, r: Random): Paragraph = {
    def fallback(): Paragraph = {
      val a = data.Paragraph.randomParagraphInsertionPoint(d, r)
      Paragraph(Seq(operation.Unicode.Insert(a, data.Paragraph.random(r).serialize())), Type.Add)
    }
    r.nextInt(9) match {
      case 0 =>
        val randomFormat = r.nextInt(3) match {
          case 0 => (SpecialChar.EmphasisStart, SpecialChar.EmphasisEnd)
          case 1 => (SpecialChar.StrongStart, SpecialChar.StrongEnd)
          case 2 => (SpecialChar.StrikeThroughStart, SpecialChar.StrikeThroughEnd)
        }
        val range = data.Paragraph.randomSubparagraph(d, r)
        Paragraph(Seq(
          operation.Unicode.Insert(range.endInclusive + 1, SpecialChar.toUnicode(randomFormat._2), leftGlued = true),
          operation.Unicode.Insert(range.start, SpecialChar.toUnicode(randomFormat._1))), Type.Add)
      case 1 =>
        data.Paragraph.randomFormatted(d, r) match {
          case Some(a) => Paragraph(Seq(
            operation.Unicode.Delete(IntRange(a.endInclusive - SpecialChar.Size + 1, a.endInclusive)),
            operation.Unicode.Delete(IntRange(a.start, a.start + SpecialChar.Size - 1))), Type.Delete)
          case None => fallback()
        }
        // remove a format
      case 2 =>
        // add title/image to a subparagraph
        val randomFormat = r.nextInt(3) match {
          case 0 => (SpecialChar.EmphasisStart, SpecialChar.EmphasisEnd)
          case 1 => (SpecialChar.StrongStart, SpecialChar.StrongEnd)
          case 2 => (SpecialChar.StrikeThroughStart, SpecialChar.StrikeThroughEnd)
        }
        val range = data.Paragraph.randomSubparagraph(d, r)
        Paragraph(Seq(
          operation.Unicode.Insert(range.endInclusive + 1, SpecialChar.toUnicode(randomFormat._2), leftGlued = true),
          operation.Unicode.Insert(range.start, SpecialChar.toUnicode(randomFormat._1))), Type.Add)
      case 3 =>
        // remove title/image to a subparagraph
        data.Paragraph.randomTitleOrLink(d, r) match {
          case Some((a, t)) => Paragraph(Seq(
            operation.Unicode.Delete(IntRange(a.start, a.start + SpecialChar.Size - 1)),
            operation.Unicode.Delete(IntRange(t.start - SpecialChar.Size, a.endInclusive))),
            Type.Delete)
          case None => fallback()
        }
      case 4 =>
        // change title/image url/title
        data.Paragraph.randomTitleOrLink(d, r) match {
          case Some((_, t)) => Paragraph(Seq(operation.Unicode.ReplaceAtomic(t, data.Unicode(r.nextString(10)))), Type.AddDelete)
          case None => fallback()
        }
      case 5 =>
        // delete a subparagrpah
        val range = data.Paragraph.randomSubparagraph(d, r)
        Paragraph(Seq(operation.Unicode.Delete(range)), Type.Delete)
      case 6 =>
        // insert a subparagraph
        fallback()
      case 7 =>
        // delete inside unicode
        data.Paragraph.randomCoded(d, r) match {
          case Some(a) if a.size > SpecialChar.Size * 2 =>
            val len = r.nextInt(a.size - SpecialChar.Size * 2)
            val start = r.nextInt(a.size - SpecialChar.Size * 2 - len)
            Paragraph(Seq(operation.Unicode.Delete(IntRange(start, start + len - 1))), Type.Add)
          case None => fallback()
        }
      case 8 =>
        // insert inside unicode
        data.Paragraph.randomCoded(d, r) match {
          case Some(a) => Paragraph(Seq(operation.Unicode.Insert(
            SpecialChar.Size + a.start + r.nextInt(a.size - SpecialChar.Size * 2), data.Unicode(r.nextInt(10).toString))), Type.Add)
          case None => fallback()
        }
    }
  }

  override def apply(cs: TRANSACTION, model: data.Paragraph): data.Paragraph = {
    data.Paragraph.parse(cs.foldLeft(model.serialize()) { (m, c) =>
      Unicode.apply(c.u, m)
    })
  }

  override def applyT(cs: Seq[TRANSACTION], model: data.Paragraph): data.Paragraph = {
    data.Paragraph.parse(cs.foldLeft(model.serialize()) { (m, t) =>
      t.foldLeft(m) { (m, c) => Unicode.apply(c.u, m) }
    })
  }
}
