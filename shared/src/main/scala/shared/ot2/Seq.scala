package shared.ot2


import boopickle.{PickleState, Pickler, UnpickleState}
import shared._
import shared.util._
import operation.Operation

import scala.util.Random
import com.softwaremill.quicklens._

object Seq {

  sealed trait Conflict[T, C] {}

  object Conflict {
    case class Asymmetry[T, C]() extends Conflict[T, C]
    case class WinnerDeletesLoser[T, C]() extends Conflict[T, C]
    case class LoserDeletesWinner[T, C]() extends Conflict[T, C]
    case class Child[T, C](c: C) extends Conflict[T, C]
  }

  val stackDepth = new ThreadLocal[Int] {
    override def initialValue(): Int = 0
  }
}

class Seq[T, O <: Operation[T], C](val cot: Ot[T, O, C]) extends Ot[scala.Seq[T], operation.Seq[T, O], Seq.Conflict[T, C]]{

  type RebaseResult = Rebased[Seq.Conflict[T, C], (Option[operation.Seq[T, O]], Option[operation.Seq[T, O]])]

  override def rebase(winner: operation.Seq[T, O], loser: operation.Seq[T, O]): RebaseResult = {
    def addDelete(add: operation.Seq.Insert[T, O], delete: operation.Seq.Delete[T, O], addIsWinner: Boolean): RebaseResult = {
      val wat = add.at
      val wc = add.childs
      val lfrom = delete.start
      val lto = delete.endInclusive
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(if (addIsWinner) Seq.Conflict.LoserDeletesWinner() else Seq.Conflict.WinnerDeletesLoser()), (
          None,
          Some(operation.Seq.Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.size, 0)
        Rebased(Set.empty, (
          Some(operation.Seq.Insert(wat0, wc)),
          Some(operation.Seq.Delete(lfrom + ld, lto + ld))
        ))
      }
    }
    def addChild(add: operation.Seq.Insert[T, O], child: operation.Seq.Child[T, O]): RebaseResult = {
      val lat0 = if (add.at <= child.at) child.at + add.childs.size else child.at
      Rebased(Set.empty, (Some(add), Some(child.copy(at = lat0))))
    }

    def deleteChild(delete: operation.Seq.Delete[T, O], child: operation.Seq.Child[T, O], deleteIsWinner: Boolean): RebaseResult = {
      if (child.at < delete.start) {
        Rebased(Set.empty, (Some(delete), Some(child)))
      } else if (child.at >= delete.start && child.at <= delete.endInclusive) {
        Rebased(
          Set(if (deleteIsWinner) Seq.Conflict.WinnerDeletesLoser() else Seq.Conflict.LoserDeletesWinner()),
          (
            Some(delete),
            None
          ))
      } else {
        Rebased(Set.empty, (Some(delete), Some(child.copy(at = child.at - delete.size))))
      }
    }
    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))

    (winner, loser) match {
      case (w@operation.Seq.Insert(wat, wc), l@operation.Seq.Insert(lat, lc)) =>
        if (wat == lat) {
          val at = wat
          Rebased(Set(Seq.Conflict.Asymmetry()), (
            Some(operation.Seq.Insert(at + lc.size, wc)),
            Some(loser)
          ))
        } else if (wat > lat) {
          Rebased(Set.empty, (Some(w.modify(_.at).using(_ + lc.size)), Some(l)))
        } else {
          Rebased(Set.empty, (Some(w), Some(l.modify(_.at).using(_ + wc.size))))
        }
      case (a@operation.Seq.Insert(_, _), l@operation.Seq.Delete(_, _)) =>
        addDelete(a, l, addIsWinner = true)
      case (a@operation.Seq.Insert(_, _), c@operation.Seq.Child(_, _)) =>
        addChild(a, c)
      case (d@operation.Seq.Delete(_, _), a@operation.Seq.Insert(_, _)) =>
        reverse(addDelete(a, d, addIsWinner = false))
      case (operation.Seq.Delete(wfrom, wto), operation.Seq.Delete(lfrom, lto)) =>
        val ws = Segment(wfrom, wto)
        val ls = Segment(lfrom, lto)
        val wp = transformDeletingSegmentAfterDeleted(ls, ws).map(a => operation.Seq.Delete[T, O](a.from, a.to))
        val lp = transformDeletingSegmentAfterDeleted(ws, ls).map(a => operation.Seq.Delete[T, O](a.from, a.to))
        Rebased(Set.empty, (wp, lp))
      case (d@operation.Seq.Delete(_, _), c@operation.Seq.Child(_, _)) =>
        deleteChild(d, c, deleteIsWinner = true)
      case (c@operation.Seq.Child(_, _), a@operation.Seq.Insert(_, _)) =>
        reverse(addChild(a, c))
      case (c@operation.Seq.Child(_, _), d@operation.Seq.Delete(_, _)) =>
        reverse(deleteChild(d, c, deleteIsWinner = false))
      case (w@operation.Seq.Child(wat, wop), l@operation.Seq.Child(lat, lop)) =>
        if (wat == lat) {
          val c = cot.rebase(wop, lop)
          Rebased(
            c.conflicts.map(a => Seq.Conflict.Child[T, C](a)), (
              c.t._1.map(a => operation.Seq.Child[T, O](wat, a)),
              c.t._2.map(a => operation.Seq.Child[T, O](lat, a))
            ))
        } else {
          Rebased(Set.empty, (Some(w), Some(l)))
        }
    }
  }


  override def generateRandomModel(random: Random):scala.Seq[T] = {
    Seq.stackDepth.set(Seq.stackDepth.get() + 1)
    val res = if (random.nextInt(4) < Seq.stackDepth.get()) scala.Seq.empty[T] else (0 to random.nextInt(3)).map(_ => cot.generateRandomModel(random))
    Seq.stackDepth.set(Seq.stackDepth.get() - 1)
    res
  }

  override def generateRandomChange(data:scala.Seq[T], random: Random): operation.Seq[T, O] = {
    if (random.nextBoolean() && data.nonEmpty) {
      val i = random.nextInt(data.size)
      val d = data(i)
      operation.Seq.Child(i, cot.generateRandomChange(d, random))
    } else if (random.nextBoolean() || data.isEmpty) {
      operation.Seq.Insert(random.nextInt(data.length + 1), generateRandomModel(random))
    } else {
      val (end, start) = maxMin(random.nextInt(data.length), random.nextInt(data.length))
      operation.Seq.Delete(start, end)
    }
  }

  override val dataPickler: Pickler[scala.Seq[T]] = new Pickler[scala.Seq[T]] {
    override def pickle(obj:scala.Seq[T])(implicit state: PickleState): Unit = {
      state.enc.writeInt(obj.size)
      obj.foreach(a => cot.dataPickler.pickle(a))
    }
    override def unpickle(implicit state: UnpickleState):scala.Seq[T] = {
      val a = state.dec.readInt
      (0 until a).map(_ => cot.dataPickler.unpickle(state))
    }
  }

  override val operationPickler: Pickler[operation.Seq[T, O]] = new Pickler[operation.Seq[T, O]] {
    override def pickle(obj: operation.Seq[T, O])(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case operation.Seq.Insert(at, childs) =>
          writeInt(-1)
          writeInt(at)
          dataPickler.pickle(childs)
        case operation.Seq.Delete(from, to) =>
          writeInt(-2)
          writeInt(from)
          writeInt(to)
        case operation.Seq.Child(at, op) =>
          writeInt(at)
          cot.operationPickler.pickle(op)
      }
    }

    override def unpickle(implicit state: UnpickleState): operation.Seq[T, O] = {
      import state.dec._
      val a = readInt
      a match {
        case -1 =>
          operation.Seq.Insert(readInt, dataPickler.unpickle(state))
        case -2 =>
          operation.Seq.Delete(readInt, readInt)
        case _ =>
          operation.Seq.Child(a, cot.operationPickler.unpickle(state))
      }
    }
  }
}



