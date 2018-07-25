package model.operation

import model.data._
import model.{data, _}
import model.operation.Type.Type
import model.range.IntRange

import scala.util.Random


/**
  */
// LATER a xml like api? basically what we implemented is a OT for xml with finite attributes. but current implementation is actually OK... so maybe later
case class Rich(private [model] val u: Seq[Unicode], override val ty: Type) extends Operation[data.Rich] {

  def transform(a: mode.Content): Option[mode.Content] = u.foldLeft(Some(a) : Option[mode.Content]) {(s, u) => u.transform(s) }

  override def apply(d: data.Rich): data.Rich =
    data.Rich.parse(Unicode.apply(u, d.serialize()))

}

object Rich extends OperationObject[data.Rich, Rich] {
  def merge(op1: Rich, op2: Rich, ty: Type): Rich = {
    Rich(op1.u ++ op2.u, ty)
  }

  def merge(op1: Seq[Rich], ty: Type): Rich = {
    Rich(op1.flatMap(_.u), ty)
  }


  def wrapAsCoded(a: data.Unicode, r: IntRange, deli: SpecialChar.Delimitation): Rich = {
    Rich(
      Seq(
        Unicode.Insert(r.start, deli.wrap(a)),
        Unicode.Delete(r.moveBy(r.size + deli.wrapSizeOffset))
      ), Type.AddDelete)
  }


  def wrap(a: IntRange, d: SpecialChar.Delimitation): operation.Rich = wrapNonOverlappingOrderedRanges(Seq(a), d)

  def wrapNonOverlappingOrderedRanges(soc: Seq[IntRange], deli: SpecialChar.Delimitation): operation.Rich = {
    Rich(soc.reverse.map(a => {
      Unicode.Surround(a, data.Unicode(deli.start), data.Unicode(deli.attributes :+ deli.end))
    }), Type.Add)
  }

  def deleteOrUnwrapAt(content: data.Rich, i: Int): Rich = {
    val info = content.infoSkipLeftAttributes(i)
    if (info.ty == InfoType.Special && !info.text.isAtomic) {
      operation.Rich.unwrap(info.nodeStart, info.text.asDelimited)
    } else {
      operation.Rich.delete(info.atomicRange)
    }
  }

  def unwrap(start: Int, value: Text.Delimited[Any]): operation.Rich = {
    operation.Rich(
      Seq(
        Unicode.Delete(start + value.contentSize + 1, start + value.size),
        Unicode.Delete(start, start + 1)
      ),
      Type.Delete
    )
  }


  def deleteNoneOverlappingOrderedRanges(range: Seq[IntRange]): Rich = Rich(range.reverse.map(a => Unicode.Delete(a)), Type.Delete)

  def delete(range: IntRange): Rich = deleteNoneOverlappingOrderedRanges(Seq(range))

  def insert(p: Int, unicode: data.Unicode): Rich = Rich(Seq(Unicode.Insert(p, unicode)), Type.Add)


  override val pickler: Pickler[Rich] = new Pickler[Rich] {
    override def pickle(obj: Rich)(implicit state: PickleState): Unit = {
      import state.enc._
      writeInt(obj.u.size)
      obj.u.foreach(a => Unicode.pickler.pickle(a))
      writeInt(obj.ty.id)
    }

    override def unpickle(implicit state: UnpickleState): Rich = {
      import state.dec._
      Rich((0 until readInt).map(_ => Unicode.pickler.unpickle), Type(readInt))
    }
  }

  /**
    * this generate random changes based on data, not invoking the practical operation handlers
    */
  override def random(d: data.Rich, r: Random): Rich = {
    def fallback(): Rich = {
      val a = randomParagraphInsertionPoint(d, r)
      Rich(Seq(operation.Unicode.Insert(a, data.Rich.random(r).serialize())), Type.Add)
    }
    val rc = r.nextInt(9)
    rc match {
      case 0 =>
        val randomFormat = SpecialChar.formatLike(r.nextInt(SpecialChar.formatLike.size))
        val range = randomSubrich(d, r)
        Rich(Seq(
          operation.Unicode.Surround(range, randomFormat.startUnicode, randomFormat.endUnicode)), Type.Add)
      case 1 =>
        randomFormatted(d, r) match {
          case Some(a) => Rich(Seq(
            operation.Unicode.Delete(IntRange(a.until - 1)),
            operation.Unicode.Delete(IntRange(a.start))), Type.Delete)
          case None => fallback()
        }
        // remove a format
      case 2 =>
        // add title to a subparagraph
        val randomFormat =
          (randomSubrich(d, r), LinkStart, UrlAttribute, TitleAttribute, LinkEnd)
        Rich(Seq(
          operation.Unicode.Surround(randomFormat._1, data.Unicode(randomFormat._2),
            data.Unicode(randomFormat._3) +
              data.Unicode("http://www.baidu.com") +
              data.Unicode(randomFormat._4) +
              data.Unicode(randomFormat._5)
          )
        ), Type.Add)
      case 3 =>
        // remove title/image to a subparagraph
        randomUrlAttributed(d, r) match {
          case Some((a, t)) => Rich(Seq(
            operation.Unicode.Delete(IntRange(t.start - 1, a.until)),
            operation.Unicode.Delete(IntRange(a.start))
          ),
            Type.Delete)
          case None => fallback()
        }
      case 4 =>
        // change title/image url/title
        randomUrlAttributed(d, r) match {
          case Some((_, t)) => Rich(Seq(operation.Unicode.ReplaceAtomic(t, data.Unicode(r.nextInt().toString))), Type.AddDelete)
          case None => fallback()
        }
      case 5 =>
        // delete a subparagrpah
        val range = randomSubrich(d, r)
        Rich(Seq(operation.Unicode.Delete(range)), Type.Delete)
      case 6 =>
        // insert a subparagraph
        fallback()
      case 7 =>
        // delete inside unicode
        randomCoded(d, r) match {
          case Some(a) if a.size > 2 =>
            val len = r.nextInt(a.size - 2)
            val start = a.start + 1 + r.nextInt(a.size - 2 - len)
            Rich(Seq(operation.Unicode.Delete(IntRange(start, start + len))), Type.Delete)
          case None => fallback()
        }
      case 8 =>
        // insert inside unicode
        randomCoded(d, r) match {
          case Some(a) => Rich(Seq(operation.Unicode.Insert(
            1 + a.start + r.nextInt(a.size - 2), data.Unicode(r.nextInt(10).toString))), Type.Add)
          case None => fallback()
        }
      case _ =>
        fallback()
    }
  }




  def randomParagraphInsertionPoint(d: data.Rich, r: Random): Int = {
    if (d.size == 0) 0
    else randomSubrich(d, r).start
  }

  private def randomUrlAttributed(d: data.Rich, r: Random): Option[(IntRange, IntRange)] = {
    val info = d.infos.zipWithIndex
    val starts = info.filter(a => a._1.ty match {
      case InfoType.Special => SpecialChar.urlAttributed.exists(j => a._1.isSpecialChar(j.start))
      case _ => false
    })
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size))
      val text = t._1.text.asDelimited
      val end = info.find(a => a._1.nodeCursor == t._1.nodeCursor && a._1.isSpecialChar(text.delimitation.end)).get._2 + 1
      val urlStart = info.find(a => a._1.nodeCursor == t._1.nodeCursor && a._1.isSpecialChar(text.attributes.head)).get._2 + 1
      val urlEnd = info.find(a => a._1.nodeCursor == t._1.nodeCursor && a._1.isSpecialChar(text.attributes(1))).get._2
      Some((IntRange(t._2, end), IntRange(urlStart, urlEnd)))
    }
  }

  private def randomFormatted(d: data.Rich, r: Random): Option[IntRange] = {
    val info = d.infos.zipWithIndex
    val starts = info.filter(a => a._1.ty match {
      case InfoType.Special => SpecialChar.formatLike.exists(j => a._1.isSpecialChar(j.start))
      case _ => false
    })
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size))
      val text = t._1.text.asDelimited
      val end = info.find(a => a._1.nodeCursor == t._1.nodeCursor  && a._1.isSpecialChar(text.delimitation.end)).get._2 + 1
      Some(IntRange(t._2, end))
    }
  }

  private def randomCoded(d: data.Rich, r: Random): Option[IntRange] = {
    val info = d.infos.zipWithIndex
    val starts = info.filter(a => a._1.ty match {
      case InfoType.Special => SpecialChar.coded.exists(j => a._1.isSpecialChar(j.start))
      case _ => false
    })
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size))
      val text = t._1.text.asCoded
      val end = info.find(a => a._1.nodeCursor == t._1.nodeCursor && a._1.isSpecialChar(text.delimitation.end)).get._2 + 1
      Some(IntRange(t._2, end))
    }
  }



  private def randomSubrich(d: data.Rich, r: Random): IntRange = {
    if (d.size == 0) {
      return IntRange(0, 0)
    }
    var i = 0
    while (i < 1000000) {
      i += 1
      val a = r.nextInt(d.size)
      val b = r.nextInt(d.size - a) + a
      val range = IntRange(a, b + 1)
      if (d.isSubRich(range))  {
        return range
      }
    }
    throw new IllegalStateException("Should return before " + d)
  }




  override def apply(cs: TRANSACTION, model: data.Rich): data.Rich = {
    data.Rich.parse(cs.foldLeft(model.serialize()) { (m, c) =>
      Unicode.apply(c.u, m)
    })
  }

  override def applyT(cs: Seq[TRANSACTION], model: data.Rich): data.Rich = {
    data.Rich.parse(cs.foldLeft(model.serialize()) { (m, t) =>
      t.foldLeft(m) { (m, c) => Unicode.apply(c.u, m) }
    })
  }
}
