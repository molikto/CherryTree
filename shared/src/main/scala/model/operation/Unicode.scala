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

    override def transform(i: mode.Content): Option[mode.Content] = Some(i match {
      case mode.Content.Insertion(s) =>
        mode.Content.Insertion(if (s < at) s else if (s > at || leftGlued) s + unicode.size else s)
      case mode.Content.Visual(a, b) =>
        val (max, min) = maxMin(a, b)
        if (at <= min) mode.Content.Visual(a + unicode.size, b + unicode.size)
        else if (at > max) mode.Content.Visual(a, b)
        else if (a > b) mode.Content.Visual(a + unicode.size, b)
        else mode.Content.Visual(a, b + unicode.size)
      case mode.Content.Normal(IntRange(min, max)) =>
        if (at <= min) mode.Content.Normal(IntRange(min + unicode.size, max + unicode.size))
        else if (at > max) i
        else mode.Content.Normal(IntRange(min, max + unicode.size))
    })
  }
  object Delete {
    def apply(l: Int, r: Int): Delete = Delete(IntRange(l, r))
  }
  case class Delete(r: IntRange) extends Unicode {
    override def ty: Type = Type.Delete
    override def apply(d: data.Unicode): data.Unicode = d.delete(r)

    override def transform(i: mode.Content): Option[mode.Content] = i match {
      case mode.Content.Insertion(k) =>
        Some(mode.Content.Insertion(if (k <= r.start) {
          k
        } else if (k > r.endInclusive) {
          k - r.size
        } else {
          k
        }))
     case mode.Content.Visual(a, b) =>
        if (a > b) {
          r.transformDeletingRangeAfterDeleted(IntRange(b, a)).map(r => mode.Content.Visual(r.endInclusive, r.start))
        } else {
          r.transformDeletingRangeAfterDeleted(IntRange(a, b)).map(r => mode.Content.Visual(r.start, r.endInclusive))
        }
      case mode.Content.Normal(range) =>
        r.transformDeletingRangeAfterDeleted(range).map(r => mode.Content.Normal(r))
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
        IntRange(range.start, range.endInclusive + sizeDiff)
      } else if (range.overlap(r)) {
        throw new IllegalArgumentException("fdsfsa")
      } else if (range.start < r.start) {
        range
      } else {
        range.moveBy(sizeDiff)
      }
    }

    override def transform(i: mode.Content): Option[mode.Content] = Some(i match {
      case mode.Content.Insertion(k) =>
        if (r.deletesCursor(k)) {
          throw new IllegalArgumentException("")
        } else if (k <= r.start) {
          i
        } else {
          mode.Content.Insertion(k + sizeDiff)
        }
      case mode.Content.Visual(a, b) =>
        val (max, min) = maxMin(a, b)
        val range = transformRange(IntRange(max, min))
        if (a < b) {
          mode.Content.Visual(range.start, range.endInclusive)
        } else {
          mode.Content.Visual(range.endInclusive, range.start)
        }
      case mode.Content.Normal(range) =>
        mode.Content.Normal(transformRange(range))
    })
  }

  case class Surround(r: IntRange, left: data.Unicode, right: data.Unicode, idempotent: Boolean = true) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.replace(r, left.join(d.slice(r)).join(right))

    def transformRange(range: IntRange): Option[IntRange] = {
      if (range == r) {
        Some(range.moveBy(left.size))
      } else if (range.contains(r)) {
        Some(IntRange(range.start, range.endInclusive + left.size + right.size))
      } else if (range.overlap(r)) {
        None
      } else if (range.start < r.start) {
        Some(range)
      } else {
        Some(range.moveBy(left.size + right.size))
      }
    }

    override def transform(i: mode.Content): Option[mode.Content] = i match {
      case mode.Content.Insertion(k) =>
        Some(mode.Content.Insertion(if (k <= r.start) k else if (k <= r.endInclusive) k + left.size else k + left.size + right.size))
      case mode.Content.Visual(a, b) =>
        val (max, min) = maxMin(a, b)
        transformRange(IntRange(max, min)).map(range => {
          if (a < b) {
            mode.Content.Visual(range.start, range.endInclusive)
          } else {
            mode.Content.Visual(range.endInclusive, range.start)
          }
        })
      case mode.Content.Normal(range) =>
        transformRange(range).map(a => mode.Content.Normal(a))
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
      Delete(start, end)
    }
  }
  
  override val pickler: Pickler[Unicode] = new Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Insert(at, childs, leftGlued) =>
          writeInt(if (leftGlued) 1 else 0)
          writeInt(at)
          writeString(childs.toString)
        case Delete(range) =>
          writeInt(2)
          IntRange.pickler.pickle(range)
        case ReplaceAtomic(range, unicode) =>
          writeInt(3)
          IntRange.pickler.pickle(range)
          writeString(unicode.toString)
        case Move(r, at) =>
          writeInt(4)
          IntRange.pickler.pickle(r)
          writeInt(at)
        case Surround(r, start, end, id) =>
          writeInt(if (id) 5 else 6)
          IntRange.pickler.pickle(r)
          writeString(start.toString)
          writeString(end.toString)
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
