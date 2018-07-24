package model.operation

import model._
import Type.Type
import model.ot.Rebased
import model.ot.Unicode.free
import util._
import model.range.IntRange

import scala.util.Random


sealed trait Unicode extends Operation[data.Unicode] {
  def transform(i: mode.Content): Option[mode.Content]
  def transform(i: Option[mode.Content]): Option[mode.Content] = i.flatMap(transform)
}

object Unicode extends OperationObject[data.Unicode, Unicode] {

  case class Insert(at: Int, unicode: data.Unicode, leftGlued: Boolean = false) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.insert(at, unicode)

    def transformRange(r: IntRange): IntRange = r match {
      case IntRange(start, until) =>
        if (at <= start) IntRange(start + unicode.size, until + unicode.size)
        else if (at >= until) r
        else IntRange(start, until + unicode.size)
    }

    override def transform(i: mode.Content): Option[mode.Content] = Some(i match {
      case mode.Content.RichInsert(s) =>
        mode.Content.RichInsert(if (s < at) s else if (s > at || leftGlued) s + unicode.size else s)
      case mode.Content.RichVisual(a, b) =>
        mode.Content.RichVisual(transformRange(a), transformRange(b))
      case mode.Content.RichNormal(r) => mode.Content.RichNormal(transformRange(r))
      case a => a
    })
  }
  object Delete {
    def apply(l: Int, r: Int): Delete = Delete(IntRange(l, r))
  }
  case class Delete(r: IntRange) extends Unicode {
    override def ty: Type = Type.Delete
    override def apply(d: data.Unicode): data.Unicode = d.delete(r)

    def transformRange(range: IntRange): Option[IntRange] =
      r.transformDeletingRangeAfterDeleted(range)

    override def transform(i: mode.Content): Option[mode.Content] = i match {
      case mode.Content.RichInsert(k) =>
        Some(mode.Content.RichInsert(if (k <= r.start) {
          k
        } else if (k >= r.until) {
          k - r.size
        } else {
          k
        }))
     case mode.Content.RichVisual(a, b) =>
       (transformRange(a), transformRange(b)) match {
         case (Some(aa), Some(bb)) => Some(mode.Content.RichVisual(aa, bb))
         case _ => None
       }
      case mode.Content.RichNormal(range) =>
        transformRange(range).map(r => mode.Content.RichNormal(r))
      case a => Some(a)
    }
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
  case class ReplaceAtomic(r: IntRange, unicode: data.Unicode) extends Unicode {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Unicode): data.Unicode = d.replace(r, unicode)
    def sizeDiff: Int =  unicode.size - r.size

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

    override def transform(i: mode.Content): Option[mode.Content] = Some(i match {
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
      case a => a
    })
  }

  case class Surround(r: IntRange, left: data.Unicode, right: data.Unicode, idempotent: Boolean = true) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.surround(r, left, right)

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

    override def transform(i: mode.Content): Option[mode.Content] = i match {
      case mode.Content.RichInsert(k) =>
        Some(mode.Content.RichInsert(if (k <= r.start) k else if (k < r.until) k + left.size else k + left.size + right.size))
      case mode.Content.RichVisual(a, b) =>
        (transformRange(a), transformRange(b)) match {
          case (Some(aa), Some(bb)) => Some(mode.Content.RichVisual(aa, bb))
          case _ => None
        }
      case mode.Content.RichNormal(range) =>
        transformRange(range).map(a => mode.Content.RichNormal(a))
      case a => Some(a)
    }
  }

  case class Move(r: IntRange, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(d: data.Unicode): data.Unicode = d.move(r, at)

    override def transform(i: mode.Content): Option[mode.Content] = ???
  }

  /**
    * replace is not generated for this, because it is used for some structural data
    */
  override def random(d: data.Unicode, r: Random): Unicode = {
    if (r.nextBoolean() || d.isEmpty) {
      Insert(r.nextInt(d.size + 1), data.Unicode(r.nextLong().toString))
    } else {
      val (end, start) = maxMin(r.nextInt(d.size), r.nextInt(d.size))
      Delete(start, end + 1)
    }
  }
  
  override val pickler: Pickler[Unicode] = new Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Insert(at, childs, leftGlued) =>
          writeInt(if (leftGlued) 1 else 0)
          writeInt(at)
          writeString(childs.str)
        case Delete(range) =>
          writeInt(2)
          IntRange.pickler.pickle(range)
        case ReplaceAtomic(range, unicode) =>
          writeInt(3)
          IntRange.pickler.pickle(range)
          writeString(unicode.str)
        case Move(r, at) =>
          writeInt(4)
          IntRange.pickler.pickle(r)
          writeInt(at)
        case Surround(r, start, end, id) =>
          writeInt(if (id) 5 else 6)
          IntRange.pickler.pickle(r)
          writeString(start.str)
          writeString(end.str)
      }
    }
    override def unpickle(implicit state: UnpickleState): Unicode = {
      import state.dec._
      val k = readInt
      k match {
        case 0 | 1 =>
          Insert(readInt, data.Unicode(readString), leftGlued = k == 1)
        case 2 =>
          Delete(IntRange.pickler.unpickle)
        case 3 =>
          ReplaceAtomic(IntRange.pickler.unpickle, data.Unicode(readString))
        case 4 =>
          Move(IntRange.pickler.unpickle, readInt)
        case 5 | 6 =>
          Surround(IntRange.pickler.unpickle, data.Unicode(readString), data.Unicode(readString), idempotent = k == 5)
      }
    }
  }
}
