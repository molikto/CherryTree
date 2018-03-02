package shared.ot


import com.softwaremill.quicklens._


sealed trait SeqOperation[T, OP <: OtOperation[T]] extends OtOperation[Seq[T]] {
}

object SeqOperation {
  case class Add[T, OP <: OtOperation[T]](at: Int, childs: Seq[T]) extends SeqOperation[T, OP] {
    override def information: Int = Information.Add
  }
  case class Delete[T, OP <: OtOperation[T]](from: Int, to: Int) extends SeqOperation[T, OP] {
    assert(from >= 0 && to >= from)
    override def information: Int = Information.Delete
  }
  case class Child[T, OP <: OtOperation[T]](at: Int, op: OP) extends SeqOperation[T, OP] {
    override def information: Int = op.information
  }
}

sealed trait SeqConflict[T] {}

object SeqConflict {
  case class Asymmetry[T]() extends SeqConflict[T]
  case class WinnerDeletesLoser[T]() extends SeqConflict[T]
  case class LoserDeletesWinner[T]() extends SeqConflict[T]
}

class SeqOt[T, O <: OtOperation[T], C](val cot: Ot[T, O, C]) extends Ot[Seq[T], SeqOperation[T, O], SeqConflict[T]]{

  override def apply(c: SeqOperation[T, O], data: Seq[T]): Seq[T] = c match {
    case SeqOperation.Add(at, cs) => data.take(at) ++ cs ++ data.drop(at)
    case SeqOperation.Delete(from, to) => data.take(from) ++ data.drop(to + 1)
    case SeqOperation.Child(at, op) => data.take(at) ++ Seq(cot.apply(op, data(at))) ++ data.drop(at + 1)
  }

  override def rebase(winner: SeqOperation[T, O], loser: SeqOperation[T, O]):
    Rebased[SeqConflict[T], (Option[SeqOperation[T, O]], Option[SeqOperation[T, O]])] = {
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
      case (SeqOperation.Add(wat, wc), SeqOperation.Delete(lfrom, lto)) =>
        ???
      case (SeqOperation.Add(wat, wc), SeqOperation.Child(lat, lop)) =>
        ???
      case (SeqOperation.Delete(wfrom, wto), SeqOperation.Add(lat, lc)) =>
        ???
      case (SeqOperation.Delete(wfrom, wto), SeqOperation.Delete(lfrom, lto)) =>
        ???
      case (SeqOperation.Delete(wfrom, wto), SeqOperation.Child(lat, lop)) =>
        ???
      case (SeqOperation.Child(wat, wop), SeqOperation.Add(lat, lc)) =>
        ???
      case (SeqOperation.Child(wat, wop), SeqOperation.Delete(lfrom, lto)) =>
        ???
      case (SeqOperation.Child(wat, wop), SeqOperation.Child(lat, lop)) =>
        ???
    }
  }

  override val dataSerializer: Serializer[Seq[T]] = ???
  override val operationSerializer: Serializer[SeqOperation[T, O]] = ???
}



