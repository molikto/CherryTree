package model.operation

import model._
import Type.Type
import boopickle._
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
  case class Move(r: IntRange, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(d: data.Unicode): data.Unicode = d.move(r, at)
  }

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
        case Move(r, at) =>
          writeInt(2)
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
          Move(IntRange.pickler.unpickle, readInt)
      }
    }
  }
}
