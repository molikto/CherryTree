package model.ot

import model._
import model.data.SpecialChar
import com.softwaremill.quicklens._
import model.operation.Unicode.Insert
import model.operation.Unicode.Delete
import model.range.IntRange

object Unicode extends Ot[data.Unicode, operation.Unicode, conflict.Unicode] {

  type RebaseResult = Rebased[conflict.Unicode, (Seq[operation.Unicode], Seq[operation.Unicode])]
  
  


  override def rebase(winner: operation.Unicode, loser: operation.Unicode): RebaseResult = {
    def addDelete(add: Insert, delete: Delete, deleteConflict: => conflict.Unicode): RebaseResult = {
      val wat = add.at
      val wc = add.unicode
      val lfrom = delete.r.start
      val lto = delete.r.until
      if (delete.r.deletesCursor(add.at)) {
        Rebased(Set(deleteConflict), (
          Seq.empty,
          Seq(Delete(delete.r.start, delete.r.until + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.r.size, 0)
        free(
          Insert(wat0, wc),
          Delete(lfrom + ld, lto + ld)
        )
      }
    }

    def reverse(res: RebaseResult): RebaseResult = Rebased(res.conflicts, (res.t._2, res.t._1))

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
          free(w.modify(_.at).using(_ + lc.size), l)
        } else {
          free(w, l.modify(_.at).using(_ + wc.size))
        }
      case (a@Insert(_, _, _), l@Delete(_)) =>
        addDelete(a, l, conflict.Unicode.LoserDeletesWinner())
      case (d@Delete(_), a@Insert(_, _, _)) =>
        reverse(addDelete(a, d, conflict.Unicode.WinnerDeletesLoser()))
      case (Delete(ws), Delete(ls)) =>
        val wp = ls.transformDeletingRangeAfterDeleted(ws).map(a => Delete(a)).toSeq
        val lp = ws.transformDeletingRangeAfterDeleted(ls).map(a => Delete(a)).toSeq
        free(wp, lp)
    }
  } 
}
