package model.operation

import model._
import Type.Type
import util._
import model.range.IntRange

import scala.util.Random


sealed trait Unicode extends Operation[data.Unicode]

object Unicode extends OperationObject[data.Unicode, Unicode] {
  case class Insert(at: Int, unicode: data.Unicode) extends Unicode {
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
  }
  case class Move(r: IntRange, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(d: data.Unicode): data.Unicode = d.move(r, at)
  }

  /**
    * replace is not generated for this, because it is used for some structural data
    */
  override def random(d: data.Unicode, random: Random): Unicode = {
    if (random.nextBoolean() || d.isEmpty) {
      Insert(random.nextInt(d.size + 1), data.Unicode(random.nextLong().toString))
    } else {
      val (end, start) = maxMin(random.nextInt(d.size), random.nextInt(d.size))
      Delete(start, end)
    }
  }
  
  override val pickler: Pickler[Unicode] = new Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Insert(at, childs) =>
          writeInt(0)
          writeInt(at)
          writeString(childs.toString)
        case Delete(range) =>
          writeInt(1)
          IntRange.pickler.pickle(range)
        case ReplaceAtomic(range, unicode) =>
          writeInt(2)
          IntRange.pickler.pickle(range)
          writeString(unicode.toString)
        case Move(r, at) =>
          writeInt(3)
          IntRange.pickler.pickle(r)
          writeInt(at)
      }
    }
    override def unpickle(implicit state: UnpickleState): Unicode = {
      import state.dec._
      readInt match {
        case 0 =>
          Insert(readInt, data.Unicode(readString))
        case 1 =>
          Delete(IntRange.pickler.unpickle)
        case 2 =>
          ReplaceAtomic(IntRange.pickler.unpickle, data.Unicode(readString))
        case 3 =>
          Move(IntRange.pickler.unpickle, readInt)
      }
    }
  }
}
