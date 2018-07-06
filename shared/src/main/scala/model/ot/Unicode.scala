package model.ot

import model._
import com.softwaremill.quicklens._

import operation.Unicode._

// LATER we currently don't generate any move operations, and we haven't handle it in our code
object Unicode extends Ot[data.Unicode, operation.Unicode, conflict.Unicode] {


  type RebaseResult = Rebased[conflict.Unicode, (Option[operation.Unicode], Option[operation.Unicode])]


  override def rebase(winner: operation.Unicode, loser: operation.Unicode): RebaseResult = {
    def addDelete(add: Insert, delete: Delete, addIsWinner: Boolean): RebaseResult = {
      val wat = add.at
      val wc = add.unicode
      val lfrom = delete.r.start
      val lto = delete.r.endInclusive
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(if (addIsWinner) conflict.Unicode.LoserDeletesWinner() else conflict.Unicode.WinnerDeletesLoser()), (
          None,
          Some(Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.r.size, 0)
        Rebased(Set.empty, some(
          Insert(wat0, wc),
          Delete(lfrom + ld, lto + ld)
        ))
      }
    }

    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))



    // TODO replaceAtomic
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
        addDelete(a, l, addIsWinner = true)
      case (d@Delete(_), a@Insert(_, _, _)) =>
        reverse(addDelete(a, d, addIsWinner = false))
      case (Delete(ws), Delete(ls)) =>
        val wp = ls.transformDeletingRangeAfterDeleted(ws).map(a => Delete(a.start, a.endInclusive))
        val lp = ws.transformDeletingRangeAfterDeleted(ls).map(a => Delete(a.start, a.endInclusive))
        Rebased(Set.empty, (wp, lp))
      case (Delete(_), Move(_, _)) =>
        ???
      case (Insert(_, _, _), Move(_, _)) =>
        ???
      case (Move(_, _), Delete(_)) =>
        ???
      case (Move(_, _), Insert(_, _, _)) =>
        ???
      case (Move(wr, wa), Move(lr, la)) =>
        ???
    }
  }
}

