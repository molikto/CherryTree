package model.ot

import model._
import com.softwaremill.quicklens._
import model.range.IntRange
import operation.Unicode._

// LATER we currently don't generate any move operations, and we haven't handle it in our code
object Unicode extends Ot[data.Unicode, operation.Unicode, conflict.Unicode] {


  type RebaseResult = Rebased[conflict.Unicode, (Seq[operation.Unicode], Seq[operation.Unicode])]


  override def rebase(winner: operation.Unicode, loser: operation.Unicode): RebaseResult = {
    def addDelete(add: Insert, delete: Delete, deleteConflict: => conflict.Unicode): RebaseResult = {
      val wat = add.at
      val wc = add.unicode
      val lfrom = delete.r.start
      val lto = delete.r.endInclusive
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(deleteConflict), (
          Seq.empty,
          Seq(Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.r.size, 0)
        Rebased(Set.empty, some(
          Insert(wat0, wc),
          Delete(lfrom + ld, lto + ld)
        ))
      }
    }

    def deleteReplaceAtomic(delete: Delete, replace: ReplaceAtomic, deleteConflict: => conflict.Unicode): RebaseResult = {
      if (delete.r.contains(replace.r)) {
        Rebased(Set(deleteConflict), (
          Seq(Delete(delete.r.start, delete.r.endInclusive + replace.sizeDiff)),
          Seq.empty
        ))
      } else if (delete.r.overlap(replace.r)) {
        throw new IllegalArgumentException()
      } else if (delete.r.start < replace.r.start) {
        free(delete, replace.modify(_.r).using(_.moveBy(-delete.r.size)))
      } else {
        free(delete.modify(_.r).using(_.moveBy(replace.sizeDiff)), replace)
      }
    }

    def insertReplaceAtomic(insert: Insert, replace: ReplaceAtomic): RebaseResult = {
      if (insert.at > replace.r.start && insert.at <= replace.r.endInclusive) {
        throw new IllegalArgumentException()
      } else if (insert.at <= replace.r.start) {
        free(insert, replace.modify(_.r).using(_.moveBy(insert.unicode.size)))
      } else {
        free(insert.modify(_.at).using(_ + replace.sizeDiff), replace)
      }
    }

    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))

    (winner, loser) match {
      case (w@Insert(wat, wc, wl), l@Insert(lat, lc, ll)) =>
        if (wat == lat) {
          if (ll) {
            // if second is left glued, then the first does't matter
            Rebased(if (wl == ll) Set(conflict.Unicode.Asymmetry()) else Set.empty, some(w.modify(_.at).using(_ + lc.size), l))
          } else {
            Rebased(if (wl == ll) Set(conflict.Unicode.Asymmetry()) else Set.empty, some(w, l.modify(_.at).using(_ + wc.size)))
          }
        } else if (wat > lat) {
          Rebased(Set.empty, some(w.modify(_.at).using(_ + lc.size), l))
        } else {
          Rebased(Set.empty, some(w, l.modify(_.at).using(_ + wc.size)))
        }
      case (a@Insert(_, _, _), l@Delete(_)) =>
        addDelete(a, l, conflict.Unicode.LoserDeletesWinner())
      case (d@Delete(_), a@Insert(_, _, _)) =>
        reverse(addDelete(a, d, conflict.Unicode.WinnerDeletesLoser()))
      case (Delete(ws), Delete(ls)) =>
        val wp = ls.transformDeletingRangeAfterDeleted(ws).map(a => Delete(a.start, a.endInclusive)).toSeq
        val lp = ws.transformDeletingRangeAfterDeleted(ls).map(a => Delete(a.start, a.endInclusive)).toSeq
        Rebased(Set.empty, (wp, lp))
      case (d@Delete(_), r@ReplaceAtomic(_, _)) =>
        deleteReplaceAtomic(d, r, conflict.Unicode.WinnerDeletesLoser())
      case (i@Insert(_, _, _), r@ReplaceAtomic(_, _)) =>
        insertReplaceAtomic(i, r)
      case (r@ReplaceAtomic(_, _), d@Delete(_)) =>
        reverse(deleteReplaceAtomic(d, r, conflict.Unicode.LoserDeletesWinner()))
      case (r@ReplaceAtomic(_, _), i@Insert(_, _, _)) =>
        reverse(insertReplaceAtomic(i, r))
      case (w@ReplaceAtomic(ws, _), l@ReplaceAtomic(ls, _)) =>
        if (ws == ls) {
          Rebased(Set(conflict.Unicode.WinnerDeletesLoser()),
            (Seq(w.modify(_.r).using(a => IntRange(a.start, a.endInclusive + l.sizeDiff))),
            Seq.empty))
        } else if (ws.overlap(ls)) {
          throw new IllegalArgumentException()
        } else if (ws.start < ls.start) {
            free(w, l.modify(_.r).using(_.moveBy(w.sizeDiff)))
        } else {
            free(w.modify(_.r).using(_.moveBy(l.sizeDiff)), l)
        }
        // LATER move related
      case (Delete(_), Move(_, _)) =>
        ???
      case (Insert(_, _, _), Move(_, _)) =>
        ???
      case (ReplaceAtomic(_, _), Move(_, _)) =>
        ???
      case (Move(_, _), Delete(_)) =>
        ???
      case (Move(_, _), Insert(_, _, _)) =>
        ???
      case (Move(_, _), ReplaceAtomic(_, _)) =>
        ???
      case (Move(wr, wa), Move(lr, la)) =>
        ???
    }
  }
}

