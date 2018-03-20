package shared.ot


import boopickle.{PickleState, Pickler, UnpickleState}
import com.softwaremill.quicklens._
import shared.util.maxMin

import scala.util.Random


sealed trait SeqOperation[T, OP <: OtOperation[T]] extends OtOperation[Seq[T]] {
}

object SeqOperation {
  case class Add[T, OP <: OtOperation[T]](at: Int, childs: Seq[T]) extends SeqOperation[T, OP] {
    override def information: Int = Information.Add
  }
  case class Delete[T, OP <: OtOperation[T]](from: Int, to: Int) extends SeqOperation[T, OP] {
    assert(from >= 0 && to >= from)
    override def information: Int = Information.Delete
    def count = to - from + 1
  }
  case class Child[T, OP <: OtOperation[T]](at: Int, op: OP) extends SeqOperation[T, OP] {
    override def information: Int = op.information
  }
}

sealed trait SeqConflict[T, C] {}

object SeqConflict {
  case class Asymmetry[T, C]() extends SeqConflict[T, C]
  case class WinnerDeletesLoser[T, C]() extends SeqConflict[T, C]
  case class LoserDeletesWinner[T, C]() extends SeqConflict[T, C]
  case class Child[T, C](c: C) extends SeqConflict[T, C]
}

class SeqOt[T, O <: OtOperation[T], C](val cot: Ot[T, O, C]) extends Ot[Seq[T], SeqOperation[T, O], SeqConflict[T, C]]{

  override def apply(c: SeqOperation[T, O], data: Seq[T]): Seq[T] = c match {
    case SeqOperation.Add(at, cs) => data.take(at) ++ cs ++ data.drop(at)
    case SeqOperation.Delete(from, to) => data.take(from) ++ data.drop(to + 1)
    case SeqOperation.Child(at, op) => data.take(at) ++ Seq(cot.apply(op, data(at))) ++ data.drop(at + 1)
  }


  type RebaseResult = Rebased[SeqConflict[T, C], (Option[SeqOperation[T, O]], Option[SeqOperation[T, O]])]

  override def rebase(winner: SeqOperation[T, O], loser: SeqOperation[T, O]): RebaseResult = {
    def addDelete(add: SeqOperation.Add[T, O], delete: SeqOperation.Delete[T, O], addIsWinner: Boolean): RebaseResult = {
      val wat = add.at
      val wc = add.childs
      val lfrom = delete.from
      val lto = delete.to
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(if (addIsWinner) SeqConflict.LoserDeletesWinner() else SeqConflict.WinnerDeletesLoser()), (
          None,
          Some(SeqOperation.Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.count, 0)
        Rebased(Set.empty, (
          Some(SeqOperation.Add(wat0, wc)),
          Some(SeqOperation.Delete(lfrom + ld, lto + ld))
        ))
      }
    }
    def addChild(add: SeqOperation.Add[T, O], child: SeqOperation.Child[T, O]): RebaseResult = {
      val lat0 = if (add.at <= child.at) child.at + add.childs.size else child.at
      Rebased(Set.empty, (Some(add), Some(child.copy(at = lat0))))
    }

    def deleteChild(delete: SeqOperation.Delete[T, O], child: SeqOperation.Child[T, O], deleteIsWinner: Boolean): RebaseResult = {
      if (child.at < delete.from) {
        Rebased(Set.empty, (Some(delete), Some(child)))
      } else if (child.at >= delete.from && child.at <= delete.to) {
        Rebased(
          Set(if (deleteIsWinner) SeqConflict.WinnerDeletesLoser() else SeqConflict.LoserDeletesWinner()),
          (
            Some(delete),
            None
          ))
      } else {
        Rebased(Set.empty, (Some(delete), Some(child.copy(at = child.at - delete.count))))
      }
    }
    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))

    (winner, loser) match {
      case (w@SeqOperation.Add(wat, wc), l@SeqOperation.Add(lat, lc)) =>
        if (wat == lat) {
          val at = wat
          Rebased(Set(SeqConflict.Asymmetry()), (
            Some(SeqOperation.Add(at + lc.size, wc)),
            Some(loser)
          ))
        } else if (wat > lat) {
          Rebased(Set.empty, (Some(w.modify(_.at).using(_ + lc.size)), Some(l)))
        } else {
          Rebased(Set.empty, (Some(w), Some(l.modify(_.at).using(_ + wc.size))))
        }
      case (a@SeqOperation.Add(_, _), l@SeqOperation.Delete(_, _)) =>
        addDelete(a, l, addIsWinner = true)
      case (a@SeqOperation.Add(_, _), c@SeqOperation.Child(_, _)) =>
        addChild(a, c)
      case (d@SeqOperation.Delete(_, _), a@SeqOperation.Add(_, _)) =>
        reverse(addDelete(a, d, addIsWinner = false))
      case (SeqOperation.Delete(wfrom, wto), SeqOperation.Delete(lfrom, lto)) =>
        val ws = Segment(wfrom, wto)
        val ls = Segment(lfrom, lto)
        val wp = transformDeletingSegmentAfterDeleted(ls, ws).map(a => SeqOperation.Delete[T, O](a.from, a.to))
        val lp = transformDeletingSegmentAfterDeleted(ws, ls).map(a => SeqOperation.Delete[T, O](a.from, a.to))
        Rebased(Set.empty, (wp, lp))
      case (d@SeqOperation.Delete(_, _), c@SeqOperation.Child(_, _)) =>
        deleteChild(d, c, deleteIsWinner = true)
      case (c@SeqOperation.Child(_, _), a@SeqOperation.Add(_, _)) =>
        reverse(addChild(a, c))
      case (c@SeqOperation.Child(_, _), d@SeqOperation.Delete(_, _)) =>
        reverse(deleteChild(d, c, deleteIsWinner = false))
      case (w@SeqOperation.Child(wat, wop), l@SeqOperation.Child(lat, lop)) =>
        if (wat == lat) {
          val c = cot.rebase(wop, lop)
          Rebased(
            c.conflicts.map(a => SeqConflict.Child[T, C](a)), (
              c.t._1.map(a => SeqOperation.Child[T, O](wat, a)),
              c.t._2.map(a => SeqOperation.Child[T, O](lat, a))
            ))
        } else {
          Rebased(Set.empty, (Some(w), Some(l)))
        }
    }
  }


  override def generateRandomData(random: Random): Seq[T] =
    (0 to random.nextInt(10)).map(_ => cot.generateRandomData(random))

  override def generateRandomChange(data: Seq[T], random: Random): SeqOperation[T, O] = {
    if (random.nextBoolean()) {
      val i = random.nextInt(data.size)
      val d = data(i)
      SeqOperation.Child(i, cot.generateRandomChange(d, random))
    } else if (random.nextBoolean() || data.isEmpty) {
      SeqOperation.Add(random.nextInt(data.length), generateRandomData(random))
    } else {
      val (end, start) = maxMin(random.nextInt(data.length), random.nextInt(data.length))
      SeqOperation.Delete(start, end)
    }
  }

  override val dataPickler: Pickler[Seq[T]] = new Pickler[Seq[T]] {
    override def pickle(obj: Seq[T])(implicit state: PickleState): Unit = {
      state.enc.writeInt(obj.size)
      obj.foreach(a => cot.dataPickler.pickle(a))
    }
    override def unpickle(implicit state: UnpickleState): Seq[T] = {
      (0 until state.dec.readInt).map(_ => cot.dataPickler.unpickle)
    }
  }

  override val operationPickler: Pickler[SeqOperation[T, O]] = new Pickler[SeqOperation[T, O]] {
    override def pickle(obj: SeqOperation[T, O])(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case SeqOperation.Add(at, childs) =>
          writeInt(-1)
          writeInt(at)
          dataPickler.pickle(childs)
        case SeqOperation.Delete(from, to) =>
          writeInt(-2)
          writeInt(from)
          writeInt(to)
        case SeqOperation.Child(at, op) =>
          writeInt(at)
          cot.operationPickler.pickle(op)
      }
    }

    override def unpickle(implicit state: UnpickleState): SeqOperation[T, O] = {
      import state.dec._
      val a = readInt
      a match {
        case -1 =>
          SeqOperation.Add(readInt, dataPickler.unpickle)
        case -2 =>
          SeqOperation.Delete(readInt, readInt)
        case _ =>
          SeqOperation.Child(a, cot.operationPickler.unpickle)
      }
    }
  }
}



