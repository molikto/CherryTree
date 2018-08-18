package model.operation

import model._
import Type.Type
import model.data.UnicodeReader
import model.ot.Rebased
import model.ot.Unicode.free
import util._
import model.range.IntRange
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.util.Random


sealed trait Unicode extends Operation[data.Unicode] {
  def translate(start: Int): Unicode

  override type This = Unicode

  private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean)
  private[model] def transformRichMaybeBad(i: (mode.Content.Rich, Boolean)): (mode.Content.Rich, Boolean) = {
    val res = transformRichMaybeBad(i._1)
    (res._1, res._2 || i._2)
  }
}

object Unicode extends OperationObject[data.Unicode, Unicode] {

  case class Insert(at: Int, unicode: data.Unicode, leftGlued: Boolean = false) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.insert(at, unicode)


    override def translate(start: Int): Unicode = copy(at = at + start)

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
          val reader = new UnicodeReader(unicode)
          mode.Content.RichNormal(data.Rich(data.Text.parseAll(reader)).rangeBeginning)
        } else {
          mode.Content.RichNormal(transformRange(r))
        }
    }, false)

    override def reverse(d: data.Unicode): Unicode = Delete(at, at + unicode.size)

    override def merge(before: Any): Option[Unicode] = before match {
      case Insert(a, u2, _) if IntRange(a, a + u2.size).containsInsertion(at) =>
        if (a == at) {
          Some(Insert(a, unicode + u2))
        } else {
          Some(Insert(a, u2.replace(IntRange(at - a, at - a), unicode)))
        }
      case _ => None
    }

    override def isEmpty: Boolean = unicode.isEmpty

  }
  object Delete {
    def apply(l: Int, r: Int): Delete = Delete(IntRange(l, r))
  }
  case class Delete(r: IntRange) extends Unicode {
    override def ty: Type = Type.Delete
    override def apply(d: data.Unicode): data.Unicode = d.delete(r)

    override def translate(start: Int): Unicode = copy(r = r.moveBy(start))

    def transformRange(range: IntRange): Option[IntRange] =
      r.transformDeletingRangeAfterDeleted(range)

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
       (transformRange(a), transformRange(b)) match {
         case (Some(aa), Some(bb)) => (mode.Content.RichVisual(aa, bb), false)
         case (Some(aa), None) => (mode.Content.RichNormal(aa), true)
         case (None, Some(aa)) => (mode.Content.RichNormal(aa), true)
         case _ => (mode.Content.RichInsert(r.start), true)
       }
      case mode.Content.RichNormal(range) =>
        transformRange(range).map(r => (mode.Content.RichNormal(r) : mode.Content.Rich, false)).getOrElse(
          (mode.Content.RichInsert(r.start), true)
        )
    }

    override def reverse(d: data.Unicode): Unicode = Insert(r.start, d.slice(r))

    override def merge(before: Any): Option[Unicode] = before match {
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
  case class ReplaceAtomic(r: IntRange, unicode: data.Unicode) extends Unicode {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Unicode): data.Unicode = d.replace(r, unicode)
    def sizeDiff: Int =  unicode.size - r.size

    override def translate(start: Int): Unicode = copy(r = r.moveBy(start))

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
    }, false)

    override def reverse(d: data.Unicode): Unicode = ReplaceAtomic(IntRange(r.start, r.start + unicode.size), d.slice(r))

    override def merge(before: Any): Option[Unicode] = before match {
      case ReplaceAtomic(rr, uu) if r.start == rr.start =>
        assert(r == IntRange(r.start, r.start +  uu.length))
        Some(ReplaceAtomic(rr, unicode))
      case _ => None
    }

    override def isEmpty: Boolean = false
  }

  case class Surround(r: IntRange, left: data.Unicode, right: data.Unicode, idempotent: Boolean = true) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.surround(r, left, right)

    override def translate(start: Int): Unicode = copy(r = r.moveBy(start))

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
    }, false)

    def reverse2: Seq[operation.Unicode] = {
      // ---- **** ----
      Seq(Delete(r.start, r.start + left.size),
        Delete(r.until, r.until + right.size))
    }

    override def reverse(d: data.Unicode): Unicode = throw new NotImplementedError("This is ugly!!!!")

    override def merge(before: Any): Option[Unicode] = None

    override def isEmpty: Boolean = false
  }

  case class Move(r: IntRange, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(d: data.Unicode): data.Unicode = d.move(r, at)

    override def translate(start: Int): Unicode = copy(r = r.moveBy(start))

    override private[model] def transformRichMaybeBad(i: mode.Content.Rich): (mode.Content.Rich, Boolean) = throw new IllegalAccessError("We don't have unicode move yet")

    override def reverse(d: data.Unicode): Unicode =  throw new IllegalAccessError("We don't have unicode move yet")

    override def merge(before: Any): Option[Unicode] =  throw new IllegalAccessError("We don't have unicode move yet")

    override def isEmpty: Boolean =  throw new IllegalAccessError("We don't have unicode move yet")
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
