package model.operation

import model._
import Type.Type
import util._
import model.range.IntRange

import scala.util.Random


sealed trait Unicode extends Operation[data.Unicode]

object Unicode extends OperationObject[data.Unicode, Unicode] {

  case class Insert(at: Int, unicode: data.Unicode, leftGlued: Boolean = false) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.insert(at, unicode)
  }
  object Delete {
    def apply(l: Int, r: Int): Delete = Delete(IntRange(l, r))
  }
  case class Delete(r: IntRange) extends Unicode {
    override def ty: Type = Type.Delete
    override def apply(d: data.Unicode): data.Unicode = d.delete(r)
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
  }

  // LATER a xml like api? basically what we implemented is a OT for xml with finite attributes.
  case class Surround(r: IntRange, left: data.Unicode, right: data.Unicode, idempotent: Boolean = true) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.replace(r, left.join(d.slice(r)).join(right))
  }

  case class Move(r: IntRange, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(d: data.Unicode): data.Unicode = d.move(r, at)
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
