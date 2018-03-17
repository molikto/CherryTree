package shared.ot


import com.softwaremill.quicklens._


sealed trait OtStringOperation extends OtOperation[String] {
}

object OtStringOperation {
  case class Add(at: Int, childs: String) extends OtStringOperation {
    // TODO should we allow this??
    //assert(childs.length > 0)
    override def information: Int = Information.Add
  }
  case class Delete(from: Int, to: Int) extends OtStringOperation {
    assert(from >= 0 && to >= from)
    override def information: Int = Information.Delete
    def count: Int = to - from + 1
  }
}

sealed trait OtStringConflict {}

object OtStringConflict {
  case class Asymmetry() extends OtStringConflict
  case class WinnerDeletesLoser() extends OtStringConflict
  case class LoserDeletesWinner() extends OtStringConflict
}

case class OtStringSelection(from: Int, to: Int) {
  assert(from >= 0 && to >= from)
}

object OtStringDoc extends Doc[String, OtStringOperation, OtStringConflict, OtStringSelection] {

  override def apply(c: OtStringOperation, data: String): String = c match {
    case OtStringOperation.Add(at, cs) => data.take(at) ++ cs ++ data.drop(at)
    case OtStringOperation.Delete(from, to) => data.take(from) ++ data.drop(to + 1)
  }

  type RebaseResult = Rebased[OtStringConflict, (Option[OtStringOperation], Option[OtStringOperation])]

  override def rebase(winner: OtStringOperation, loser: OtStringOperation): RebaseResult = {
    def addDelete(add: OtStringOperation.Add, delete: OtStringOperation.Delete, addIsWinner: Boolean): RebaseResult = {
      val wat = add.at
      val wc = add.childs
      val lfrom = delete.from
      val lto = delete.to
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(if (addIsWinner) OtStringConflict.LoserDeletesWinner() else OtStringConflict.WinnerDeletesLoser()), (
          None,
          Some(OtStringOperation.Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.count, 0)
        Rebased(Set.empty, (
          Some(OtStringOperation.Add(wat0, wc)),
          Some(OtStringOperation.Delete(lfrom + ld, lto + ld))
        ))
      }
    }
    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))

    (winner, loser) match {
      case (w@OtStringOperation.Add(wat, wc), l@OtStringOperation.Add(lat, lc)) =>
        if (wat == lat) {
          val at = wat
          Rebased(Set(OtStringConflict.Asymmetry()), (
            Some(OtStringOperation.Add(at + lc.size, wc)),
            Some(loser)
          ))
        } else if (wat > lat) {
          Rebased(Set.empty, (Some(w.modify(_.at).using(_ + lc.size)), Some(l)))
        } else {
          Rebased(Set.empty, (Some(w), Some(l.modify(_.at).using(_ + wc.size))))
        }
      case (a@OtStringOperation.Add(_, _), l@OtStringOperation.Delete(_, _)) =>
        addDelete(a, l, addIsWinner = true)
      case (d@OtStringOperation.Delete(_, _), a@OtStringOperation.Add(_, _)) =>
        reverse(addDelete(a, d, addIsWinner = false))
      case (OtStringOperation.Delete(wfrom, wto), OtStringOperation.Delete(lfrom, lto)) =>
        val ws = Segment(wfrom, wto)
        val ls = Segment(lfrom, lto)
        val wp = transformAfterDeleted(ls, ws).map(a => OtStringOperation.Delete(a.from, a.to))
        val lp = transformAfterDeleted(ws, ls).map(a => OtStringOperation.Delete(a.from, a.to))
        Rebased(Set.empty, (wp, lp))
    }
  }

  override def apply(op: OtStringOperation, sel: OtStringSelection): Option[OtStringSelection] = ???
}



