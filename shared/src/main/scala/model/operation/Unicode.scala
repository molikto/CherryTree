package model.operation

import boopickle.{PickleState, Pickler, UnpickleState}
import model.{data, mode}
import model.operation.Type.Type
import model.range.IntRange

import scala.util.Random
import util._

sealed trait Unicode extends Operation[data.Unicode] {
  def translate(a: Int): Unicode
  override type This = Unicode

  def toEncodedSeq: EncodedSeq
}

object Unicode extends OperationObject[data.Unicode, Unicode] {

  case class Insert(at: Int, unicode: data.Unicode, leftGlued: Boolean = false) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.insert(at, unicode)


    override def toString: String = s"$at+$unicode"

    override def translate(start: Int): Unicode = copy(at = at + start)

    override def reverse(d: data.Unicode): Unicode = Delete(at, at + unicode.size)

    override def merge(before: Any, whitespace: Boolean): Option[Unicode] =
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

    override def toEncodedSeq: EncodedSeq = EncodedSeq.Insert(at, data.EncodedSeq(unicode), leftGlued)
  }
  object Delete {
    def apply(l: Int, r: Int): Delete = Delete(IntRange(l, r))
  }

  case class Delete(r: IntRange) extends Unicode {

    override def toString: String = s"${r.start}-${r.size}"
    override def ty: Type = Type.Delete
    override def apply(d: data.Unicode): data.Unicode = d.delete(r)

    override def translate(start: Int): Unicode = copy(r = r.moveBy(start))

    override def reverse(d: data.Unicode): Unicode = Insert(r.start, d.slice(r))

    override def merge(before: Any, whitespace: Boolean): Option[Unicode] = before match {
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

    override def toEncodedSeq: EncodedSeq = EncodedSeq.Delete(r)
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
          data.Unicode.pickler.pickle(childs)
        case Delete(range) =>
          writeInt(2)
          IntRange.pickler.pickle(range)
      }
    }
    override def unpickle(implicit state: UnpickleState): Unicode = {
      import state.dec._
      val k = readInt
      k match {
        case 0 | 1 =>
          Insert(readInt, data.Unicode.pickler.unpickle, leftGlued = k == 1)
        case 2 =>
          Delete(IntRange.pickler.unpickle)
      }
    }
  }

}
