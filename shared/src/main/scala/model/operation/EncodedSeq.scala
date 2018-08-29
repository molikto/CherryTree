package model.operation

import model._
import Type.Type
import model.data.EncodedSeqReader
import model.ot.Rebased
import util._
import model.range.IntRange
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.util.Random


sealed trait EncodedSeq extends Operation[data.EncodedSeq] {
  def translate(start: Int): EncodedSeq

  override type This = EncodedSeq

  private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean)
  private[model] def transformRichMaybeBad(i: (mode.Content.Rich, Boolean)): (mode.Content.Rich, Boolean) = {
    val res = transformRichMaybeBad(i._1)
    (res._1, res._2 || i._2)
  }
}

object EncodedSeq extends OperationObject[data.EncodedSeq, EncodedSeq] {

  case class Insert(at: Int, unicode: data.EncodedSeq, leftGlued: Boolean = false) extends EncodedSeq {
    override def ty: Type = Type.Add
    override def apply(d: data.EncodedSeq): data.EncodedSeq = d.insert(at, unicode)


    override def toString: String = s"$at+$unicode"

    override def translate(start: Int): EncodedSeq = copy(at = at + start)

    def transformRange(r: IntRange): IntRange = r match {
      case IntRange(start, until) =>
        if (at <= start) IntRange(start + unicode.size, until + unicode.size)
        else if (at >= until) r
        else IntRange(start, until + unicode.size)
    }

    override private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean) = (i match {
      case mode.Content.RichInsert(s) =>
        mode.Content.RichInsert(if (s < at) s else if (s > at || leftGlued) s + unicode.size else s)
      case mode.Content.RichVisual(a, b) =>
        mode.Content.RichVisual(transformRange(a), transformRange(b))
      case mode.Content.RichNormal(r) =>
        if (r.isEmpty) {
          assert(at == 0)
          // this only happens when the document is empty
          // LATER this is also hacky!!!!
          val reader = new EncodedSeqReader(unicode)
          mode.Content.RichNormal(data.Rich(data.Text.parseAll(reader)).rangeBeginning)
        } else {
          mode.Content.RichNormal(transformRange(r))
        }
      case mode.Content.RichAttributeSubMode(range, rich) =>
        val ran = if (at < range.start) {
          range.moveBy(unicode.size)
        } else if (at >= range.start && at <= range.until) {
          IntRange(range.start, range.until + unicode.size)
        } else if (at > range.until) {
          range
        } else {
          throw new Exception("Not handled case")
        }
        val (ri, _) = transformRichMaybeBad(rich)
        mode.Content.RichAttributeSubMode(ran, ri)
      case mode.Content.RichCodeSubMode(range, code, rich) =>
        var insertInside = Int.MaxValue
        val ran = if (at < range.start) {
          range.moveBy(unicode.size)
        } else if (at >= range.start && at <= range.until) {
          insertInside = at - range.start
          IntRange(range.start, range.until + unicode.size)
        } else if (at > range.until) {
          range
        } else {
          throw new Exception("Not handled case")
        }
        val co = code.copy(pos = if (code.pos <= insertInside) code.pos else code.pos + unicode.size)
        val (ri, _) = transformRichMaybeBad(rich)
        mode.Content.RichCodeSubMode(ran, co, ri)
    }, false)

    override def reverse(d: data.EncodedSeq): EncodedSeq = Delete(at, at + unicode.size)

    override def merge(before: Any, whitespace: Boolean): Option[EncodedSeq] =
      if (whitespace && unicode.containsSpace) {
        None
      } else {
        before match {
          case Insert(a, u2, _) if IntRange(a, a + u2.size).containsInsertion(at) =>
            if (a == at) {
              Some(Insert(a, unicode + u2))
            } else {
              Some(Insert(a, u2.replace(IntRange(at - a, at - a), unicode)))
            }
          case _ => None
        }
      }

    override def isEmpty: Boolean = unicode.isEmpty

  }
  object Delete {
    def apply(l: Int, r: Int): Delete = Delete(IntRange(l, r))
  }
  case class Delete(r: IntRange) extends EncodedSeq {

    override def toString: String = s"${r.start}-${r.size}"
    override def ty: Type = Type.Delete
    override def apply(d: data.EncodedSeq): data.EncodedSeq = d.delete(r)

    override def translate(start: Int): EncodedSeq = copy(r = r.moveBy(start))

    def transformAtomicRange(range: IntRange): Option[IntRange] = {
      if (range.start == r.start || r.until == range.until) {
        None
      } else {
        r.transformDeletingRangeAfterDeleted(range)
      }
    }

    override private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean) = i match {
      case mode.Content.RichInsert(k) =>
        if (k <= r.start) {
          (i, false)
        } else if (k >= r.until) {
          (mode.Content.RichInsert(k - r.size), false)
        } else {
          (mode.Content.RichInsert(r.start), true)
        }
     case mode.Content.RichVisual(a, b) =>
       (transformAtomicRange(a), transformAtomicRange(b)) match {
         case (Some(aa), Some(bb)) => (mode.Content.RichVisual(aa, bb), false)
         case (Some(aa), None) => (mode.Content.RichNormal(aa), true)
         case (None, Some(aa)) => (mode.Content.RichNormal(aa), true)
         case _ => (mode.Content.RichInsert(r.start), true)
       }
      case mode.Content.RichNormal(range) =>
        transformAtomicRange(range).map(r => (mode.Content.RichNormal(r) : mode.Content.Rich, false)).getOrElse(
          (mode.Content.RichInsert(r.start), true)
        )
      case sub: mode.Content.RichSubMode =>
        val range = sub.range
        val rich = sub.modeBefore
        val (ri, bad) = transformRichMaybeBad(rich)
        // if range is deleted we dismiss current one... what ever
        val bad2 = bad || transformAtomicRange(IntRange(range.start - 1, range.start)).isEmpty
        if (bad2) {
          (ri, bad2)
        } else {
          var deleteInside: IntRange = null
          val ran = if (r.until <= range.start) {
            range.moveBy(-r.size)
          } else if (r.start >= range.until) {
            range
          } else if (range.contains(r)) {
            deleteInside = r.moveBy(range.start)
            IntRange(range.start, range.until - r.size)
          } else {
            throw new Exception("Not handled case")
          }
          sub match {
            case c: mode.Content.RichCodeSubMode =>
              val code = c.code
              val co = if (deleteInside != null) deleteInside.transformAfterDeleted(code.pos).getOrElse(deleteInside.start) else code.pos
              (mode.Content.RichCodeSubMode(ran, code.copy(pos = co), ri), false)
            case attr: mode.Content.RichAttributeSubMode =>
              (attr.copyWithRange(ran, ri), false)
          }
        }
    }

    override def reverse(d: data.EncodedSeq): EncodedSeq = Insert(r.start, d.slice(r))

    override def merge(before: Any, whitespace: Boolean): Option[EncodedSeq] = before match {
      case Delete(r2) if r.containsInsertion(r2.start) =>
        Some(Delete(IntRange(r.start, r.start + r2.size + r.size)))
//      case Insert(at, u, _) =>
//        val rr = IntRange(at, at + u.size)
//        if (rr.contains(r)) {
//
//        } else if (r.contains(rr)) {
//
//        }
      case _ => None
    }

    override def isEmpty: Boolean = r.isEmpty
  }

  /**
    * unlike insert and delete, if two replace happens on the same range, then
    * replace will not perform two insert
    *
    * this happens if part of the data is considered "atomic"
    *
    * so overlapping replace cannot have different range, also insertion inside
    * and delete part of a replace is not allowed
    */
  case class ReplaceAtomic(r: IntRange, unicode: data.EncodedSeq) extends EncodedSeq {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.EncodedSeq): data.EncodedSeq = d.replace(r, unicode)
    def sizeDiff: Int =  unicode.size - r.size

    override def translate(start: Int): EncodedSeq = copy(r = r.moveBy(start))

    def transformRange(range: IntRange): IntRange = {
      if (range.contains(r)) {
        IntRange(range.start, range.until + sizeDiff)
      } else if (range.overlap(r)) {
        throw new IllegalStateException("ReplaceAtomic should not be called with overlapping range")
      } else if (range.start < r.start) {
        range
      } else {
        range.moveBy(sizeDiff)
      }
    }

    override private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean) = (i match {
      case mode.Content.RichInsert(k) =>
        if (r.deletesCursor(k)) {
          throw new IllegalStateException("ReplaceAtomic should not be called with insertion inside")
        } else if (k <= r.start) {
          i
        } else {
          mode.Content.RichInsert(k + sizeDiff)
        }
      case mode.Content.RichVisual(a, b) =>
        mode.Content.RichVisual(transformRange(a), transformRange(b))
      case mode.Content.RichNormal(range) =>
        mode.Content.RichNormal(transformRange(range))
      case sub: mode.Content.RichSubMode =>
        val range = sub.range
        val ran = if (r.until < range.start) {
          range.moveBy(sizeDiff)
        } else if (r.start > range.until) {
          range
        } else {
          throw new Exception("Not handled case")
        }
        sub.copyWithRange(ran, transformRichMaybeBad(sub.modeBefore)._1)
    }, false)

    override def reverse(d: data.EncodedSeq): EncodedSeq = ReplaceAtomic(IntRange(r.start, r.start + unicode.size), d.slice(r))

    override def merge(before: Any, whitespace: Boolean): Option[EncodedSeq] = before match {
      case ReplaceAtomic(rr, uu) if r.start == rr.start =>
        assert(r == IntRange(r.start, r.start + uu.size))
        Some(ReplaceAtomic(rr, unicode))
      case _ => None
    }

    override def isEmpty: Boolean = false
  }

  case class Surround(r: IntRange, left: data.EncodedSeq, right: data.EncodedSeq, idempotent: Boolean = true) extends EncodedSeq {
    override def ty: Type = Type.Add
    override def apply(d: data.EncodedSeq): data.EncodedSeq = d.surround(r, left, right)

    override def translate(start: Int): EncodedSeq = copy(r = r.moveBy(start))

    def transformRange(range: IntRange): Option[IntRange] = {
      if (range == r) {
        Some(range.moveBy(left.size))
      } else if (range.contains(r)) {
        Some(IntRange(range.start, range.until + left.size + right.size))
      } else if (r.contains(range)) {
        Some(IntRange(range.start + left.size, range.until + left.size))
      } else if (range.overlap(r)) {
        None
      } else if (range.start < r.start) {
        Some(range)
      } else {
        Some(range.moveBy(left.size + right.size))
      }
    }

    override private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean) = (i match {
      case mode.Content.RichInsert(k) =>
        mode.Content.RichInsert(if (k <= r.start) k else if (k < r.until) k + left.size else k + left.size + right.size)
      case mode.Content.RichVisual(a, b) =>
        (transformRange(a), transformRange(b)) match {
          case (Some(aa), Some(bb)) => mode.Content.RichVisual(aa, bb)
          case _ => throw new IllegalArgumentException("Should not usually happen")
        }
      case mode.Content.RichNormal(range) =>
        if (range.isEmpty) {
          mode.Content.RichNormal(IntRange(0, 1)) // LATER we know all our surround is some surround by special char!!
        } else {
          transformRange(range).map(a => mode.Content.RichNormal(a)).get
        }
      case sub: mode.Content.RichSubMode =>
        val range = sub.range
        val ran = if (r.until < range.start) {
          range.moveBy(left.size)
        } else if (r.start > range.until) {
          range
        } else if (r.start < range.start && r.until > range.until) {
          range.moveBy(left.size)
        } else {
          throw new Exception("Not handled case")
        }
        sub.copyWithRange(ran, transformRichMaybeBad(sub.modeBefore)._1)
    }, false)

    def reverse2: Seq[operation.EncodedSeq] = {
      // ---- **** ----
      Seq(Delete(r.start, r.start + left.size),
        Delete(r.until, r.until + right.size))
    }

    override def reverse(d: data.EncodedSeq): EncodedSeq = throw new NotImplementedError("This is ugly!!!!")

    override def merge(before: Any, whitespace: Boolean): Option[EncodedSeq] = None

    override def isEmpty: Boolean = false
  }

  /**
    * replace is not generated for this, because it is used for some structural data
    */
  override def random(d: data.EncodedSeq, r: Random): EncodedSeq = {
    if (r.nextBoolean() || d.isEmpty) {
      Insert(r.nextInt(d.size + 1), data.EncodedSeq(r.nextLong().toString))
    } else {
      val (end, start) = maxMin(r.nextInt(d.size), r.nextInt(d.size))
      Delete(start, end + 1)
    }
  }

  override val pickler: Pickler[EncodedSeq] = new Pickler[EncodedSeq] {
    override def pickle(obj: EncodedSeq)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Insert(at, childs, leftGlued) =>
          writeInt(if (leftGlued) 1 else 0)
          writeInt(at)
          data.EncodedSeq.pickler.pickle(childs)
        case Delete(range) =>
          writeInt(2)
          IntRange.pickler.pickle(range)
        case ReplaceAtomic(range, unicode) =>
          writeInt(3)
          IntRange.pickler.pickle(range)
          data.EncodedSeq.pickler.pickle(unicode)
        case Surround(r, start, end, id) =>
          writeInt(if (id) 5 else 6)
          IntRange.pickler.pickle(r)
          data.EncodedSeq.pickler.pickle(start)
          data.EncodedSeq.pickler.pickle(end)
      }
    }
    override def unpickle(implicit state: UnpickleState): EncodedSeq = {
      import state.dec._
      val k = readInt
      k match {
        case 0 | 1 =>
          Insert(readInt, data.EncodedSeq.pickler.unpickle, leftGlued = k == 1)
        case 2 =>
          Delete(IntRange.pickler.unpickle)
        case 3 =>
          ReplaceAtomic(IntRange.pickler.unpickle, data.EncodedSeq.pickler.unpickle)
        case 5 | 6 =>
          Surround(IntRange.pickler.unpickle, data.EncodedSeq.pickler.unpickle, data.EncodedSeq.pickler.unpickle, idempotent = k == 5)
      }
    }
  }


}
