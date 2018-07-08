package model.ot

import model._
import com.softwaremill.quicklens._
import model.data.SpecialChar
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
        free(
          Insert(wat0, wc),
          Delete(lfrom + ld, lto + ld)
        )
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

    def deleteSurround(d: Delete, s: Surround, deleteConflict: => conflict.Unicode): RebaseResult = {
      // this is not a deleting range, but, it should work
      if (d.r.contains(s.r)) {
        Rebased(Set(deleteConflict), (Seq(d.copy(r = d.r.modify(_.endInclusive).using(_ + s.left.size + s.right.size))), Seq.empty))
      } else if (s.r.contains(d.r)) {
        free(d.modify(_.r).using(_.moveBy(s.left.size)), s.modify(_.r).using(_.modify(_.endInclusive).using(_ - d.r.size)))
      } else if (d.r.overlap(s.r)) {
        // still consider it free, somehow
        if (d.r.start < s.r.start) {
          // [   ( ]  )
          val leftRange = IntRange(d.r.start, s.r.start - 1)
          val rightStart = s.r.start + s.left.size
          val rightRange = IntRange(rightStart, rightStart + d.r.size - leftRange.size)
          free(Seq(
              Delete(leftRange),
              Delete(rightRange)),
            Seq(s.copy(r = IntRange(d.r.start, s.r.endInclusive - d.r.size))))
        } else {
          // (   [ )  ]
          free(Seq(
              Delete(IntRange(d.r.start, s.r.endInclusive).moveBy(s.left.size)),
              Delete(IntRange(s.r.endInclusive + 1, d.r.endInclusive).moveBy(s.left.size + s.right.size))),
              Seq(s.modify(_.r).using(_.modify(_.endInclusive).using(_ => d.r.start - 1))))
        }
      } else if (d.r.start < s.r.start){
        free(d, s.modify(_.r).using(_.moveBy(-d.r.size)))
      } else {
        free(d.modify(_.r).using(_.moveBy(s.left.size + s.right.size)), s)
      }
    }

    def insertSurround(i: Insert, s: Surround): RebaseResult = {
      if (i.at <= s.r.start) {
        free(i, s.modify(_.r).using(_.moveBy(i.unicode.size)))
      } else if (i.at <= s.r.endInclusive) {
        free(i.modify(_.at).using(_ + s.left.size), s.modify(_.r).using(_.modify(_.endInclusive).using(_ + i.unicode.size)))
      } else {
        free(i.modify(_.at).using(_ + s.left.size + s.right.size), s)
      }
    }

    def replaceAtomicSurround(r: ReplaceAtomic, s: Surround): RebaseResult = {
      if (s.r.contains(r.r)) {
        free(r.modify(_.r).using(_.moveBy(s.left.size)), s.modify(_.r).using(_.modify(_.endInclusive).using(_ + r.sizeDiff)))
      } else if (s.r.overlap(r.r)) {
        throw new IllegalArgumentException("Not atomic")
      } else if (r.r.start < s.r.start) {
        free(r, s.modify(_.r).using(_.moveBy(r.sizeDiff)))
      } else {
        free(r.modify(_.r).using(_.moveBy(s.left.size + s.right.size)), s)
      }
    }

    def reverseSurround(a: Surround): Seq[operation.Unicode] = {
      // ---- **** ----
      Seq(Delete(a.r.start, a.r.start + a.left.size - 1),
        Delete(a.r.endInclusive + a.left.size + 1, a.r.endInclusive + a.left.size + a.right.size))
    }

    def overlapSurround(s: Surround, l: Surround, sWins: Boolean): RebaseResult = {
      // s: [], l: ()
      val order = SpecialChar.surroundStartCodeInToOut
      val si = order.indexOf(s.left)
      val li = order.indexOf(l.left)
      val min = SpecialChar.surroundStartCodeInToOut(Math.min(si, li))
      val r1 = IntRange(s.r.start, l.r.start - 1)
      val r2 = IntRange(l.r.start, s.r.endInclusive)
      val r3 = IntRange(s.r.endInclusive + 1, l.r.endInclusive)
      // [   (   ]   )
      if (SpecialChar.surroundStartCodeNotSplit.contains(min)) { // cannot split
        // if cannot: [   ]   (  )
        val overall = Seq(Surround(r3, l.left, l.right), Surround(r1, s.left, s.right))
        free(
          reverseSurround(l) ++ overall,
          reverseSurround(s) ++ overall
        )
      } else {
        if (si < li || (si == li && sWins)) { // s splits
          // [   (   ]   ) ===> [    ]([    ]    )
          val seq = Seq(Surround(r1, s.left, s.right), Surround(r2.moveBy(s.left.size + s.right.size + l.left.size), s.left, s.right))
          free(
            seq,
            reverseSurround(s) ++ (l +: seq))
        } else { // r splits
          // [   (   ]   ) ===> [     (    )](    )
          val seq = Seq(Surround(r2.moveBy(s.left.size), l.left, l.right), Surround(r3.moveBy(s.left.size + l.left.size + s.right.size + l.right.size), l.left, l.right))
          free(
            reverseSurround(l) ++ (s +: seq),
            seq
          )
        }
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
        val wp = ls.transformDeletingRangeAfterDeleted(ws).map(a => Delete(a.start, a.endInclusive)).toSeq
        val lp = ws.transformDeletingRangeAfterDeleted(ls).map(a => Delete(a.start, a.endInclusive)).toSeq
        free(wp, lp)
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
      case (d@Delete(_), s@Surround(_, _, _, _)) =>
        deleteSurround(d, s, conflict.Unicode.WinnerDeletesLoser())
      case (i@Insert(_, _, _), s@Surround(_, _, _, _)) =>
        insertSurround(i, s)
      case (r@ReplaceAtomic(_, _), s@Surround(_, _, _, _)) =>
        replaceAtomicSurround(r, s)
      case (s@Surround(_, _, _, _), d@Delete(_)) =>
        reverse(deleteSurround(d, s, conflict.Unicode.LoserDeletesWinner()))
      case (s@Surround(_, _, _, _), i@Insert(_, _, _)) =>
        reverse(insertSurround(i, s))
      case (s@Surround(_, _, _, _), r@ReplaceAtomic(_, _)) =>
        reverse(replaceAtomicSurround(r, s))
      case (w@Surround(wr, ws, we, wid), l@Surround(lr, ls, le, lid)) =>
        if (wid && lid && ws == ls && we == le && wr.overlap(lr)) { // [][] is not included now, who knows
          if (w == l) {
            free(Seq.empty, Seq.empty)
          } else {
            // merge results
            val points = Seq(wr.start, wr.endInclusive, lr.start, lr.endInclusive)
            val overall = Surround(IntRange(points.min, points.max), ws, we)
            free(reverseSurround(l) :+ overall, reverseSurround(w) :+ overall)
          }
        } else {
          def wrapLInW() =
            free(w.modify(_.r).using(_.modify(_.endInclusive).using(_ + ls.size + le.size)), l.modify(_.r).using(_.moveBy(ws.size)))
          def wrapWInL() =
            free(w.modify(_.r).using(_.moveBy(ls.size)), l.modify(_.r).using(_.modify(_.endInclusive).using(_ + ws.size + we.size)))
          if (wr == lr) {
            // some order how should they be applied?
            val order = SpecialChar.surroundStartCodeInToOut
            val wi = order.indexOf(ws)
            val li = order.indexOf(ls)
            if (wi > li) {
              wrapLInW()
            } else {
              wrapWInL()
            }
          } else if (wr.contains(lr)) {
            wrapLInW()
          } else if (lr.contains(wr)) {
            wrapWInL()
          } else if (wr.overlap(lr)) {
            if  (wr.start < lr.start) {
              overlapSurround(w, l, sWins = true)
            } else {
              reverse(overlapSurround(l, w, sWins = false))
            }
          } else if  (wr.start < lr.start) {
            free(w, l.modify(_.r).using(_.moveBy(ws.size + we.size)))
          } else {
            free(w.modify(_.r).using(_.moveBy(ls.size + le.size)), l)
          }
        }
      // LATER move related
      case (Delete(_), Move(_, _)) =>
        ???
      case (Insert(_, _, _), Move(_, _)) =>
        ???
      case (ReplaceAtomic(_, _), Move(_, _)) =>
        ???
      case (Surround(_, _, _, _), Move(_, _)) =>
        ???
      case (Move(_, _), Delete(_)) =>
        ???
      case (Move(_, _), Insert(_, _, _)) =>
        ???
      case (Move(_, _), ReplaceAtomic(_, _)) =>
        ???
      case (Move(_, _), Surround(_, _, _, _)) =>
        ???
      case (Move(wr, wa), Move(lr, la)) =>
        ???
    }
  }
}

