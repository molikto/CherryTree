//package shared.ot
//
//
//import com.softwaremill.quicklens._
//
//
//sealed trait SetOperation[T, OP <: OtOperation[T]] extends OtOperation[Set[T]] {
//}
//
//object SetOperation {
//  case class Add[T, OP <: OtOperation[T]](at: Int, childs: Set[T]) extends SetOperation[T, OP] {
//    override def information: Int = Information.Add
//  }
//  case class Delete[T, OP <: OtOperation[T]](from: Int, to: Int) extends SetOperation[T, OP] {
//    assert(from >= 0 && to >= from)
//    override def information: Int = Information.Delete
//    def count = to - from + 1
//  }
//  case class Child[T, OP <: OtOperation[T]](at: Int, op: OP) extends SetOperation[T, OP] {
//    override def information: Int = op.information
//  }
//}
//
//sealed trait SetConflict[T, C] {}
//
//object SetConflict {
//  case class Asymmetry[T, C]() extends SetConflict[T, C]
//  case class WinnerDeletesLoser[T, C]() extends SetConflict[T, C]
//  case class LoserDeletesWinner[T, C]() extends SetConflict[T, C]
//  case class Child[T, C](c: C) extends SetConflict[T, C]
//}
//
//class SetOt[T, O <: OtOperation[T], C](val cot: Ot[T, O, C]) extends Ot[Set[T], SetOperation[T, O], SetConflict[T, C]]{
//
//  override def apply(c: SetOperation[T, O], data: Set[T]): Set[T] = c match {
//    case SetOperation.Add(at, cs) => data.take(at) ++ cs ++ data.drop(at)
//    case SetOperation.Delete(from, to) => data.take(from) ++ data.drop(to + 1)
//    case SetOperation.Child(at, op) => data.take(at) ++ Set(cot.apply(op, data(at))) ++ data.drop(at + 1)
//  }
//
//  type RebaseResult = Rebased[SetConflict[T, C], (Option[SetOperation[T, O]], Option[SetOperation[T, O]])]
//
//  override def rebase(winner: SetOperation[T, O], loser: SetOperation[T, O]): RebaseResult = {
//    def addDelete(add: SetOperation.Add[T, O], delete: SetOperation.Delete[T, O], addIsWinner: Boolean): RebaseResult = {
//      val wat = add.at
//      val wc = add.childs
//      val lfrom = delete.from
//      val lto = delete.to
//      if (lfrom < wat && lto >= wat) {
//        Rebased(Set(if (addIsWinner) SetConflict.LoserDeletesWinner() else SetConflict.WinnerDeletesLoser()), (
//          None,
//          Some(SetOperation.Delete(lfrom, lto + wc.size))
//        ))
//      } else {
//        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.count, 0)
//        Rebased(Set.empty, (
//          Some(SetOperation.Add(wat0, wc)),
//          Some(SetOperation.Delete(lfrom + ld, lto + ld))
//        ))
//      }
//    }
//    def addChild(add: SetOperation.Add[T, O], child: SetOperation.Child[T, O]): RebaseResult = {
//      val lat0 = if (add.at <= child.at) child.at + add.childs.size else child.at
//      Rebased(Set.empty, (Some(add), Some(child.copy(at = lat0))))
//    }
//
//    def deleteChild(delete: SetOperation.Delete[T, O], child: SetOperation.Child[T, O], deleteIsWinner: Boolean): RebaseResult = {
//      if (child.at < delete.from) {
//        Rebased(Set.empty, (Some(delete), Some(child)))
//      } else if (child.at >= delete.from && child.at <= delete.to) {
//        Rebased(
//          Set(if (deleteIsWinner) SetConflict.WinnerDeletesLoser() else SetConflict.LoserDeletesWinner()),
//          (
//            Some(delete),
//            None
//          ))
//      } else {
//        Rebased(Set.empty, (Some(delete), Some(child.copy(at = child.at - delete.count))))
//      }
//    }
//    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))
//
//    (winner, loser) match {
//      case (w@SetOperation.Add(wat, wc), l@SetOperation.Add(lat, lc)) =>
//        if (wat == lat) {
//          val at = wat
//          Rebased(Set(SetConflict.Asymmetry()), (
//            Some(SetOperation.Add(at + lc.size, wc)),
//            Some(loser)
//          ))
//        } else if (wat > lat) {
//          Rebased(Set.empty, (Some(w.modify(_.at).using(_ + lc.size)), Some(l)))
//        } else {
//          Rebased(Set.empty, (Some(w), Some(l.modify(_.at).using(_ + wc.size))))
//        }
//      case (a@SetOperation.Add(_, _), l@SetOperation.Delete(_, _)) =>
//        addDelete(a, l, addIsWinner = true)
//      case (a@SetOperation.Add(_, _), c@SetOperation.Child(_, _)) =>
//        addChild(a, c)
//      case (d@SetOperation.Delete(_, _), a@SetOperation.Add(_, _)) =>
//        reverse(addDelete(a, d, addIsWinner = false))
//      case (SetOperation.Delete(wfrom, wto), SetOperation.Delete(lfrom, lto)) =>
//        val ws = Segment(wfrom, wto)
//        val ls = Segment(lfrom, lto)
//        val wp = transformAfterDeleted(ls, ws).map(a => SetOperation.Delete[T, O](a.from, a.to))
//        val lp = transformAfterDeleted(ws, ls).map(a => SetOperation.Delete[T, O](a.from, a.to))
//        Rebased(Set.empty, (wp, lp))
//      case (d@SetOperation.Delete(_, _), c@SetOperation.Child(_, _)) =>
//        deleteChild(d, c, deleteIsWinner = true)
//      case (c@SetOperation.Child(_, _), a@SetOperation.Add(_, _)) =>
//        reverse(addChild(a, c))
//      case (c@SetOperation.Child(_, _), d@SetOperation.Delete(_, _)) =>
//        reverse(deleteChild(d, c, deleteIsWinner = false))
//      case (w@SetOperation.Child(wat, wop), l@SetOperation.Child(lat, lop)) =>
//        if (wat == lat) {
//          val c = cot.rebase(wop, lop)
//          Rebased(
//            c.conflicts.map(a => SetConflict.Child[T, C](a)), (
//              c.t._1.map(a => SetOperation.Child[T, O](wat, a)),
//              c.t._2.map(a => SetOperation.Child[T, O](lat, a))
//            ))
//        } else {
//          Rebased(Set.empty, (Some(w), Some(l)))
//        }
//    }
//  }
//
//  override val dataSerializer: Serializer[Set[T]] = ???
//  override val operationSerializer: Serializer[SetOperation[T, O]] = ???
//}
//
//
//
