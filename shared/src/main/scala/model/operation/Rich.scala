package model.operation

import model.data.Atom.CodedSpecial
import model.data.SpecialChar.Delimitation
import model.data._
import model.{data, _}
import model.operation.Type.Type
import model.range.IntRange

import scala.util.Random


/**
  */
// LATER a xml like api? basically what we implemented is a OT for xml with finite attributes. but current implementation is actually OK... so maybe later
case class Rich(private [model] val u: Seq[EncodedSeq], override val ty: Type) extends Operation[data.Rich] {


  override def toString: String = u.mkString(", ")

  def canBeSmartInsert(rich: data.Rich): Option[(data.Unicode, Int, data.Unicode)] = {
    def doIt(at: Int, unicode: data.EncodedSeq):  Option[(data.Unicode, Int, data.Unicode)]  = {
      unicode.singleUnicodeOption match {
        case Some(jj) =>
          if (at == 0) {
            return Some((data.Unicode.empty, at, jj))
          }
          val before = rich.before(at)
          if (before.text.isCoded && !before.delimitationEnd) {
            return None
          } else if (before.text.isPlain) {
            val g = before.asInstanceOf[Atom.PlainGrapheme]
            return Some((before.text.asPlain.unicode.slice(IntRange(0, g.unicodeIndex + g.size)), at, jj))
          } else {
            return Some((Rich.returnUnicodeOnNonText, at, jj))
          }
        case None =>
          None
      }
    }


    u match {
      case Seq(EncodedSeq.Insert(at, unicode, _)) =>
        doIt(at, unicode)
      case Seq(EncodedSeq.Insert(start, b, _), EncodedSeq.Delete(IntRange(startd, endd))) if startd >= start + b.size =>
        doIt(start, b)
      case _ => None
    }
  }

  def transformToCodeChange(range: IntRange): Option[Seq[Unicode]] = Some(u.foldLeft((range, Seq.empty[Unicode])) { (res, to) =>
    val range = res._1
    val se = res._2
    to match {
      case EncodedSeq.Delete(r) =>
        if (r.until <= range.start) {
          (range.moveBy(-r.size), se)
        } else if (r.start >= range.until) {
          res
        } else if (range.contains(r)) {
          (IntRange(range.start, range.until - r.size), se :+ Unicode.Delete(r.moveBy(-range.start)))
        } else {
          return None
        }
      case EncodedSeq.Insert(at, a, gl) =>
        if (at < range.start) {
          (range.moveBy(a.size), se)
        } else if (at >= range.start && at <= range.until) {
          (IntRange(range.start, range.until + a.size), se :+ Unicode.Insert(at - range.start, a.singleUnicode, gl))
        } else if (at > range.until) {
          res
        } else {
          return None
        }
      case EncodedSeq.Surround(r, left, right, _) =>
        if (r.until < range.start) {
          (range.moveBy(left.size), se)
        } else if (r.start > range.until) {
          res
        } else if (r.start < range.start && r.until > range.until) {
          (range.moveBy(left.size), se)
        } else {
          return None
        }
      case a@EncodedSeq.ReplaceAtomic(r, unicode) =>
        if (r.until < range.start) {
          (range.moveBy(a.sizeDiff), se)
        } else if (r.start > range.until) {
          res
        } else {
          return None
        }
    }
  }._2)


  def transformRich(d: data.Rich, a: mode.Content.Rich, enableModal: Boolean): (mode.Content.Rich, Boolean) = {
    val maybeBad = u.foldLeft((a, false)) {
      (s, u) => u.transformRichMaybeBad(s)
    }
    if (maybeBad._2 && enableModal) {
      val m = maybeBad._1 match {
        case mode.Content.RichInsert(i) =>
          model.mode.Content.RichNormal(apply(d).rangeAfter(i))
        case _ => maybeBad._1
      }
      if (model.debug_view) {
        println(s"bad mode to $m")
      }
      (m, true)
    } else {
      maybeBad
    }
  }

  override def apply(d: data.Rich): data.Rich =
    data.Rich.parse(EncodedSeq.apply(u, d.serialize()))

  override type This = Rich

  override def reverse(d: data.Rich): Rich = {
    Rich(u.foldLeft((d.serialize(), Seq.empty[operation.EncodedSeq])) { (s, a) =>
      a match {
        case k: EncodedSeq.Surround =>
          val op = k.reverse2
          (k(s._1), op ++ s._2)
        case a =>
          val op = a.reverse(s._1)
          (a(s._1), op +: s._2)
      }
    }._2, Type.reverse(ty))
  }

  private def mergeIfSingle(u: Seq[EncodedSeq], before: Seq[EncodedSeq], breakOnWhitespace: Boolean): Option[Rich] = {
    (before, u) match {
      case (Seq(EncodedSeq.Insert(a0, u0, g0)), Seq(EncodedSeq.Insert(a1, u1, g1))) if a1 == a0 + u0.size && (!breakOnWhitespace || !u1.containsSpace) && g0 == g1 =>
        Some(Rich(Seq(EncodedSeq.Insert(a0, u0 + u1, g0)), ty))
      case (Seq(EncodedSeq.Delete(r0)), Seq(EncodedSeq.Delete(r1))) =>
        if (r1.until == r0.start) {
          Some(Rich(Seq(EncodedSeq.Delete(r1.start, r0.until)), ty))
        } else if (r0.start == r1.start) {
          Some(Rich(Seq(EncodedSeq.Delete(r1.start, r1.until + r0.size)), ty))
        } else {
          None
        }
      case (Seq(EncodedSeq.Insert(a0, u0, g0)), Seq(EncodedSeq.Insert(a1, u1, g1), EncodedSeq.Delete(r))) if g0 == g1 && (!breakOnWhitespace || !u1.containsSpace) =>
        if (a1 >= a0 && a1 < a0 + u0.size && r.start == a1 + u1.size && r.until == a0 + u0.size + u1.size) {
          Some(Rich(Seq(
            EncodedSeq.Insert(a0, u0.slice(IntRange(0, u0.size - r.size)) + u1,g0)
          ), Type.AddDelete))
        } else {
          None
        }
      case _ =>
        None

    }
  }


  override def merge(before: Any, whitespace: Boolean): Option[Rich] =
    mergeIfSingle(EncodedSeq.merge(u), EncodedSeq.merge(before.asInstanceOf[Rich].u), breakOnWhitespace = whitespace)

  override def isEmpty: Boolean = u.forall(_.isEmpty)
}

object Rich extends OperationObject[data.Rich, Rich] {
  def fromCode(insideStart: Int, uni: Seq[Unicode]): Rich = {
    Rich(uni.map(_.toEncodedSeq.translate(insideStart)), Type.AddDelete)
  }

  def replacePlain(seq: Seq[(Int, Int, data.Unicode)]): Rich = {
    Rich(seq.reverse.flatMap(a => replacePlain0(a._1, a._2, a._3)), Type.AddDelete)
  }

  private def replacePlain0(start: Int, end: Int, b: data.Unicode): Seq[operation.EncodedSeq] = {
    if (b.isEmpty) {
      Seq(EncodedSeq.Delete(start, end))
    } else if (start == end) {
      Seq(EncodedSeq.Insert(start, data.EncodedSeq(b)))
    } else {
        Seq(
          EncodedSeq.Insert(start, data.EncodedSeq(b)),
          EncodedSeq.Delete(start + b.size, end + b.size)
        )
    }
  }

  def replacePlain(start: Int, end: Int, b: data.Unicode): Rich = {
    if (b.isEmpty) {
      Rich(Seq(EncodedSeq.Delete(start, end)), Type.Delete)
    } else if (start == end) {
      Rich(Seq(EncodedSeq.Insert(start, data.EncodedSeq(b))), Type.Add)
    } else {
      Rich(
        Seq(
          EncodedSeq.Insert(start, data.EncodedSeq(b)),
          EncodedSeq.Delete(start + b.size, end + b.size)
        ),
        Type.AddDelete
      )
    }
  }


  private[operation] val returnUnicodeOnNonText = data.Unicode("(DON'T MATCH THIS!)")
  def merge(op1: Rich, op2: Rich, ty: Type): Rich = {
    Rich(op1.u ++ op2.u, ty)
  }

  def merge(op1: Seq[Rich], ty: Type): Rich = {
    Rich(op1.flatMap(_.u), ty)
  }

  def changeAttributeAt(rich: data.Rich, textStart: Int, url: data.Unicode, title: data.Unicode): Rich = {
    val atom = rich.after(textStart)
    val text = atom.text.asDelimited
    Rich(
      Seq(
        EncodedSeq.ReplaceAtomic(text.rangeAttribute(TitleAttribute).moveBy(textStart), data.EncodedSeq(title)),
        EncodedSeq.ReplaceAtomic(text.rangeAttribute(UrlAttribute).moveBy(textStart), data.EncodedSeq(url))
      ),
      Type.AddDelete)
  }



  def wrapAsCoded(a: data.Unicode, r: IntRange, deli: SpecialChar.Delimitation): Rich = {
    Rich(
      Seq(
        EncodedSeq.Insert(r.start, deli.wrap(data.EncodedSeq(a))),
        EncodedSeq.Delete(r.moveBy(r.size + deli.wrapSizeOffset))
      ), Type.AddDelete)
  }

  def deleteTextualRange(rich: model.data.Rich, r0: IntRange): Option[(Seq[operation.Rich], IntRange, Int)] = {
    val ssss = util.last(rich.befores(r0.start).takeWhile {
      case s: Atom.Special => r0.contains(s.another.range)
      case _ => false
    }).map(_.range.start).getOrElse(r0.start)
    val uuuu = util.last(rich.afters(r0.until).takeWhile {
      case s: Atom.Special => r0.contains(s.another.range)
      case _ => false
    }).map(_.range.until).getOrElse(r0.until)
    val r = IntRange(ssss, uuuu)
    val singleSpecials = rich.singleSpecials(r)
    val reverses = singleSpecials.map(_.another)
    val ds = r.minusOrderedInside(singleSpecials.map(_.range))
    def deleteRanges(i: Seq[IntRange]) = {
      val remaining = IntRange(0, rich.size).minusOrderedInside(i)
      val posTo = if (remaining.isEmpty) {
        (IntRange(0, 0), 0) // all deleted
      } else {
        // for all remaining bits
        val (tempPos, a) = remaining.find(_.until > r.start).map(_.start max r.start)
          .map(a => rich.after(a)).map(a => (a.range, 0))
          .getOrElse((rich.before(remaining.last.until).range, 1))
        (tempPos.moveByOrZeroZero(-i.filter(_.start < tempPos.start).map(_.size).sum), a)
      }
      Some((
        Seq(operation.Rich.deleteNoneOverlappingOrderedRanges(i)),
        posTo._1, posTo._2))
    }
    if (ds.isEmpty) {
      if (singleSpecials.forall(_.delimitationStart) || singleSpecials.forall(_.delimitationEnd)) {
        deleteRanges((reverses ++ singleSpecials).map(_.range).sortBy(_.start))
      } else {
        None
      }
    } else {
      deleteRanges(ds)
    }
  }


  def wrap(a: IntRange, d: SpecialChar.Delimitation): operation.Rich = wrapNonOverlappingOrderedRanges(Seq(a), d)

  def wrapNonOverlappingOrderedRanges(soc: Seq[IntRange], deli: SpecialChar.Delimitation): operation.Rich = {
    Rich(soc.reverse.map(a => {
      EncodedSeq.Surround(a, data.EncodedSeq(deli.start), data.EncodedSeq(deli.attributes :+ deli.end))
    }), Type.Add)
  }

  def deleteOrUnwrapAt(content: data.Rich, i: Int): Rich = {
    val info = content.after(i)
    if (info.special) {
      operation.Rich.unwrap(info.textTotalIndex, info.text.asDelimited)
    } else {
      operation.Rich.delete(info.range)
    }
  }


  def wrapUnwrap(from: Int, text: Text.Delimited, to: SpecialChar.Delimitation): operation.Rich = {
    val op2 = operation.Rich.wrap(IntRange.len(from, text.size), to).u
    val op1 = operation.Rich.unwrap(from + 1, text).u
    operation.Rich(op2 ++ op1, Type.AddDelete)
  }
  def unwrap(start: Int, value: Text.Delimited): operation.Rich = {
    operation.Rich(
      Seq(
        EncodedSeq.Delete(start + value.contentSize + 1, start + value.size),
        EncodedSeq.Delete(start, start + 1)
      ),
      Type.Delete
    )
  }


  def deleteNoneOverlappingOrderedRanges(range: Seq[IntRange]): Rich = Rich(range.reverse.map(a => EncodedSeq.Delete(a)).toVector, Type.Delete)


  def delete(start: Int, until: Int): Rich = deleteNoneOverlappingOrderedRanges(Seq(IntRange(start, until)))
  def delete(range: IntRange): Rich = deleteNoneOverlappingOrderedRanges(Seq(range))

  def insert(p: Int, unicode: Delimitation): Rich = Rich(Seq(EncodedSeq.Insert(p, unicode.wrap())), Type.Add)

  def insert(p: Int, unicode: Seq[data.Text]): Rich = Rich(Seq(EncodedSeq.Insert(p, Text.serialize(unicode))), Type.Add)


  override val pickler: Pickler[Rich] = new Pickler[Rich] {
    override def pickle(obj: Rich)(implicit state: PickleState): Unit = {
      import state.enc._
      writeInt(obj.u.size)
      obj.u.foreach(a => EncodedSeq.pickler.pickle(a))
      writeInt(obj.ty.id)
    }

    override def unpickle(implicit state: UnpickleState): Rich = {
      import state.dec._
      Rich((0 until readInt).map(_ => EncodedSeq.pickler.unpickle), Type(readInt))
    }
  }

  /**
    * this generate random changes based on data, not invoking the practical operation handlers
    */
  override def random(d: data.Rich, r: Random): Rich = {
    def fallback(): Rich = {
      val a = randomParagraphInsertionPoint(d, r)
      Rich(Seq(operation.EncodedSeq.Insert(a, data.Rich.random(r).serialize())), Type.Add)
    }
    val rc = r.nextInt(9)
    rc match {
      case 0 =>
        val randomFormat = SpecialChar.formattedSplittable(r.nextInt(SpecialChar.formattedSplittable.size))
        val range = randomSubrich(d, r)
        Rich(Seq(
          operation.EncodedSeq.Surround(range, data.EncodedSeq(randomFormat.start), data.EncodedSeq(randomFormat.end))), Type.Add)
      case 1 =>
        randomFormatted(d, r) match {
          case Some(a) => Rich(Seq(
            operation.EncodedSeq.Delete(IntRange(a.until - 1)),
            operation.EncodedSeq.Delete(IntRange(a.start))), Type.Delete)
          case None => fallback()
        }
        // remove a format
      case 2 =>
        // add title to a subparagraph
        val randomFormat =
          (randomSubrich(d, r), LinkStart, UrlAttribute, TitleAttribute, LinkEnd)
        Rich(Seq(
          operation.EncodedSeq.Surround(randomFormat._1, data.EncodedSeq(randomFormat._2),
            data.EncodedSeq(randomFormat._3) +
              data.EncodedSeq("http://www.baidu.com") +
              data.EncodedSeq(randomFormat._4) +
              data.EncodedSeq(randomFormat._5)
          )
        ), Type.Add)
      case 3 =>
        // remove title/image to a subparagraph
        randomUrlAttributed(d, r) match {
          case Some((a, t)) => Rich(Seq(
            operation.EncodedSeq.Delete(IntRange(t.start - 1, a.until)),
            operation.EncodedSeq.Delete(IntRange(a.start))
          ),
            Type.Delete)
          case None => fallback()
        }
      case 4 =>
        // change title/image url
        randomUrlAttributed(d, r) match {
          case Some((_, t)) => Rich(Seq(operation.EncodedSeq.ReplaceAtomic(t, data.EncodedSeq(r.nextInt().toString))), Type.AddDelete)
          case None => fallback()
        }
      case 5 =>
        // delete a subparagrpah
        val range = randomSubrich(d, r)
        Rich(Seq(operation.EncodedSeq.Delete(range)), Type.Delete)
      case 6 =>
        // insert a subparagraph
        fallback()
      case 7 =>
        // delete inside unicode
        randomCoded(d, r) match {
          case Some(a) if a.size > 2 =>
            val len = r.nextInt(a.size - 2)
            val start = a.start + 1 + r.nextInt(a.size - 2 - len)
            Rich(Seq(operation.EncodedSeq.Delete(IntRange(start, start + len))), Type.Delete)
          case None => fallback()
        }
      case 8 =>
        // insert inside unicode
        randomCoded(d, r) match {
          case Some(a) => Rich(Seq(operation.EncodedSeq.Insert(
            1 + a.start + r.nextInt(a.size - 2), data.EncodedSeq(r.nextInt(10).toString))), Type.Add)
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
    val info = d.afters(0)
    val starts = info.filter(a => SpecialChar.urlAttributed.exists(j => a.special(j.start))).toSeq
    if (starts.isEmpty) {
      None
    } else {
      val t = starts(r.nextInt(starts.size)).asInstanceOf[Atom.Special]
      Some((t.textRange, t.text.rangeAttribute(UrlAttribute).moveBy(t.textTotalIndex)))
    }
  }

  private def randomFormatted(d: data.Rich, r: Random): Option[IntRange] = {
    val info = d.afters(0)
    val starts = info.filter(a => SpecialChar.formattedSplittable.exists(j => a.special(j.start))).toSeq
    if (starts.isEmpty) {
      None
    } else {
      Some(starts(r.nextInt(starts.size)).textRange)
    }
  }

  private def randomCoded(d: data.Rich, r: Random): Option[IntRange] = {
    val info = d.afters(0)
    val starts = info.filter(a => Seq(SpecialChar.Code, SpecialChar.LaTeX).exists(j => a.special(j.start))).toSeq
    if (starts.isEmpty) {
      None
    } else {
      Some(starts(r.nextInt(starts.size)).textRange)
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
      val g = d.isSubRich(range)
      if (g.isDefined)  {
        return g.get
      }
    }
    throw new IllegalStateException("Should return before " + d)
  }




  override def apply(cs: TRANSACTION, model: data.Rich): data.Rich = {
    data.Rich.parse(cs.foldLeft(model.serialize()) { (m, c) =>
      EncodedSeq.apply(c.u, m)
    })
  }

  override def applyT(cs: Seq[TRANSACTION], model: data.Rich): data.Rich = {
    data.Rich.parse(cs.foldLeft(model.serialize()) { (m, t) =>
      t.foldLeft(m) { (m, c) => EncodedSeq.apply(c.u, m) }
    })
  }
}
