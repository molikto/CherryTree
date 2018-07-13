package model.operation

import model.data._
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
  *
  */
// TODO a xml like api? basically what we implemented is a OT for xml with finite attributes. but current implementation is actually OK... so maybe later
case class Paragraph(private [model] val u: Seq[Unicode], override val ty: Type) extends Operation[data.Paragraph] {

  def transform(a: mode.Content): Option[mode.Content] = u.foldLeft(Some(a) : Option[mode.Content]) {(s, u) => u.transform(s) }

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
      val a = randomParagraphInsertionPoint(d, r)
      Paragraph(Seq(operation.Unicode.Insert(a, data.Paragraph.random(r).serialize())), Type.Add)
    }
    val rc = r.nextInt(9)
    rc match {
      case 0 if d.size > 0 =>
        val randomFormat = SpecialChar.formatted(r.nextInt(SpecialChar.formatted.size))
        val range = randomSubparagraph(d, r)
        Paragraph(Seq(
          operation.Unicode.Surround(range, randomFormat.startUnicode, randomFormat.endUnicode)), Type.Add)
      case 1 =>
        randomFormatted(d, r) match {
          case Some(a) => Paragraph(Seq(
            operation.Unicode.Delete(IntRange(a.endInclusive)),
            operation.Unicode.Delete(IntRange(a.start))), Type.Delete)
          case None => fallback()
        }
        // remove a format
      case 2 if d.size > 0 =>
        // add title/image to a subparagraph
        val randomFormat = r.nextInt(2) match {
          case 0 => (ImageStart, UrlAttribute, TitleAttribute, ImageEnd)
          case 1 => (LinkStart, UrlAttribute, TitleAttribute, LinkEnd)
        }
        val range = randomSubparagraph(d, r)
        Paragraph(Seq(
          operation.Unicode.Surround(range, data.Unicode(randomFormat._1),
            data.Unicode(randomFormat._2)
              .join(data.Unicode("http://www.baidu.com"))
              .join(data.Unicode(randomFormat._3))
              .join(data.Unicode(randomFormat._4))
          )
        ), Type.Add)
      case 3 =>
        // remove title/image to a subparagraph
        randomLinked(d, r) match {
          case Some((a, t)) => Paragraph(Seq(
            operation.Unicode.Delete(IntRange(t.start - 1, a.endInclusive)),
            operation.Unicode.Delete(IntRange(a.start))
          ),
            Type.Delete)
          case None => fallback()
        }
      case 4 =>
        // change title/image url/title
        randomLinked(d, r) match {
          case Some((_, t)) => Paragraph(Seq(operation.Unicode.ReplaceAtomic(t, data.Unicode(r.nextString(10)))), Type.AddDelete)
          case None => fallback()
        }
      case 5 if d.size > 0 =>
        // delete a subparagrpah
        val range = randomSubparagraph(d, r)
        Paragraph(Seq(operation.Unicode.Delete(range)), Type.Delete)
      case 6 =>
        // insert a subparagraph
        fallback()
      case 7 =>
        // delete inside unicode
        randomCoded(d, r) match {
          case Some(a) if a.size > 2 =>
            val len = r.nextInt(a.size - 2)
            val start = a.start + 1 + r.nextInt(a.size - 2 - len)
            Paragraph(Seq(operation.Unicode.Delete(IntRange(start, start + len - 1))), Type.Delete)
          case None => fallback()
        }
      case 8 =>
        // insert inside unicode
        randomCoded(d, r) match {
          case Some(a) => Paragraph(Seq(operation.Unicode.Insert(
            1 + a.start + r.nextInt(a.size - 2), data.Unicode(r.nextInt(10).toString))), Type.Add)
          case None => fallback()
        }
      case _ =>
        fallback()
    }
  }




  def randomParagraphInsertionPoint(d: data.Paragraph, r: Random): Int = {
    if (d.size == 0) 0
    else randomSubparagraph(d, r).start
  }

  private def randomLinked(d: data.Paragraph, r: Random): Option[(IntRange, IntRange)] = {
    val info = d.infos.zipWithIndex
    val starts = info.filter(a => a._1.ty match {
      case InfoType.Special => SpecialChar.linked.exists(j => a._1.isSpecialChar(j.start))
      case _ => false
    })
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size))
      val text = t._1.text.asInstanceOf[data.Text.Formatted]
      val end = info.find(a => a._1.nodePosition == t._1.nodePosition && a._1.isSpecialChar(text.specialCharEnd)).get._2
      val urlStart = info.find(a => a._1.nodePosition == t._1.nodePosition && a._1.isSpecialChar(text.attributes.head)).get._2 + 1
      val urlEnd = info.find(a => a._1.nodePosition == t._1.nodePosition && a._1.isSpecialChar(text.attributes(1))).get._2 - 1
      Some((IntRange(t._2, end), IntRange(urlStart, urlEnd)))
    }
  }

  private def randomFormatted(d: data.Paragraph, r: Random): Option[IntRange] = {
    val info = d.infos.zipWithIndex
    val starts = info.filter(a => a._1.ty match {
      case InfoType.Special => SpecialChar.formatted.exists(j => a._1.isSpecialChar(j.start))
      case _ => false
    })
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size))
      val text = t._1.text.asInstanceOf[data.Text.Formatted]
      val end = info.find(a => a._1.nodePosition == t._1.nodePosition  && a._1.isSpecialChar(text.specialCharEnd)).get._2
      Some(IntRange(t._2, end))
    }
  }

  private def randomCoded(d: data.Paragraph, r: Random): Option[IntRange] = {
    val info = d.infos.zipWithIndex
    val starts = info.filter(a => a._1.ty match {
      case InfoType.Special => SpecialChar.coded.exists(j => a._1.isSpecialChar(j.start))
      case _ => false
    })
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size))
      val text = t._1.text.asInstanceOf[data.Text.Coded]
      val end = info.find(a => a._1.nodePosition == t._1.nodePosition && a._1.isSpecialChar(text.specialCharEnd)).get._2
      Some(IntRange(t._2, end))
    }
  }


  private def randomSubparagraph(d: data.Paragraph, r: Random): IntRange = {
    if (d.size == 0) {
      throw new IllegalArgumentException()
    }
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

    val info = d.infos.zipWithIndex
    while (true) {
      val a = info(r.nextInt(d.size))
      val b = info(r.nextInt(d.size - a._2) + a._2)
      if (isValidStart(a._1) && isValidEnd(b._1)) {
        if (a._1.nodePosition == b._1.nodePosition) {
          // single item
          return IntRange(a._2, b._2)
        } else if (a._1.nodePosition.dropRight(1) == b._1.nodePosition.dropRight(1)) {
          return IntRange(a._2, b._2)
        }
      }
    }
    throw new IllegalArgumentException("")
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
