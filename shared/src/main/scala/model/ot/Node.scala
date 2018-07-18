package model.ot


import boopickle.Pickler
import model._
import com.softwaremill.quicklens._
import model.conflict.Node
import model.operation.Node
import model.range.IntRange

import scala.util.Random

object Node extends Ot[data.Node, operation.Node, conflict.Node] {

  type RebaseResult = Rebased[conflict.Node, (Seq[operation.Node], Seq[operation.Node])]


  // LATER handle move
  override def rebase(winner: operation.Node, loser: operation.Node): RebaseResult = {

    def insertMove(i: operation.Node.Insert, d: operation.Node.Move): RebaseResult = {
      ???
    }
    def deleteMove(w: operation.Node.Delete, m: operation.Node.Move, deleteConflict : => conflict.Node): RebaseResult = {
      // the location that moves to is deleted
      // the original is deleted / partially deleted
      // we don't affect each other
      ???
    }
    def insertDelete(i: operation.Node.Insert, d: operation.Node.Delete, deleteConflict : => conflict.Node): RebaseResult = {
      if (d.r.sameParent(i.at)) {
        val at = i.at.last
        val left = d.r.childs.start
        val right = d.r.childs.until - 1
        if (at <= left) {
          free(i, d.modify(_.r).using(_.modify(_.childs).using(_.moveBy(i.childs.size))))
        } else if (at > left && at <= right) {
           //[][]
          // [].....[]
          val range1 = IntRange(left, at)
          val end = right + i.childs.size
          // end.size = d.r.childs.size - range1.size = right - left + 1
          val range2 = IntRange(end + 1 - (d.r.childs.size - range1.size), end + 1)
          free(
            Seq(i.modify(_.at).using(a => a.dropRight(1) :+ left)),
            Seq(
              operation.Node.Delete(d.r.copy(childs = range2)),
              operation.Node.Delete(d.r.copy(childs = range1))
          ))
        } else {
          free(i.modify(_.at).using(a => a.dropRight(1) :+ (a.last - d.r.childs.size)), d)
        }
      } else {
        d.r.transformAfterDeleted(i.at) match {
          case Some(p) =>
            free(i.copy(at = p), d.modify(_.r).using(r => range.Node(cursor.Node.transformAfterInserted(i.at, i.childs.size, r.start), r.childs.size)))
          case None =>
            Rebased(Set(deleteConflict), (Seq.empty, Seq(d)))
        }
      }
    }
    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))
    winner match {
      case w@operation.Node.Content(wc, wo) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            if (wc == lc) {
              val r = Content.rebase(wo, lo)
              Rebased(r.conflicts.map(c => conflict.Node.Content(c)), map[operation.Content, operation.Node](r.t, a => operation.Node.Content(wc, a)))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.ReplacedByLoser()), (Seq.empty, Seq(loser)))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(w.copy(at = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), loser)
          case operation.Node.Delete(lr) =>
            lr.transformAfterDeleted(wc) match {
              case Some(p) => free(w.copy(at = p), loser)
              case None => Rebased(Set(conflict.Node.LoserDeletesWinner()), (Seq.empty, Seq(loser)))
            }
          case m@operation.Node.Move(lr, la) =>
            free(w.copy(at = lr.transformNodeAfterMoved(la, wc)), m)
        }
      case w@operation.Node.Replace(wc, _) =>
        loser match {
          case operation.Node.Content(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.ReplacedByWinner()), (Seq(winner), Seq.empty))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Seq(winner), Seq.empty))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(w.copy(at = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), loser)
          case operation.Node.Delete(lr) =>
            lr.transformAfterDeleted(wc) match {
              case Some(p) => free(w.copy(at = p), loser)
              case None => Rebased(Set(conflict.Node.LoserDeletesWinner()), (Seq.empty, Seq(loser)))
            }
          case m@operation.Node.Move(lr, la) =>
            free(w.copy(at = lr.transformNodeAfterMoved(la, wc)), m)
        }
      case w@operation.Node.Insert(wc, wcs) =>
        loser match {
          case l@operation.Node.Content(lc, _) =>
            free(w, l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Replace(lc, _) =>
            free(w, l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Insert(lc, lcs) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.Asymmetry()), some(w, l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, wc))))
            } else {
              free(w.copy(at = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
            }
          case d@operation.Node.Delete(_) =>
            insertDelete(w, d, conflict.Node.LoserDeletesWinner())
          case m@operation.Node.Move(_, _) =>
            insertMove(w, m)
        }
      case d@operation.Node.Delete(wr) =>
        loser match {
          case l@operation.Node.Content(lc, _) =>
            wr.transformAfterDeleted(lc) match {
              case Some(p) => free(winner, l.copy(at = p))
              case None => Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Seq(winner), Seq.empty))
            }
          case l@operation.Node.Replace(lc, _) =>
            wr.transformAfterDeleted(lc) match {
              case Some(p) => free(winner, l.copy(at = p))
              case None => Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Seq(winner), Seq.empty))
            }
          case i@operation.Node.Insert(_, _) =>
            reverse(insertDelete(i, d, conflict.Node.WinnerDeletesLoser()))
          case operation.Node.Delete(lr) =>
            val wp = lr.transformDeletingRangeAfterDeleted(wr).map(operation.Node.Delete).toSeq
            val lp = wr.transformDeletingRangeAfterDeleted(lr).map(operation.Node.Delete).toSeq
            Rebased(Set.empty, (wp, lp))
          case m@operation.Node.Move(_, _) =>
            deleteMove(d, m, conflict.Node.WinnerDeletesLoser())
        }
      case w@operation.Node.Move(wr, wa) =>
        loser match {
          case l@operation.Node.Content(lc, lo) =>
            free(w, l.copy(at = wr.transformNodeAfterMoved(wa, lc)))
          case l@operation.Node.Replace(lc, lo) =>
            free(w, l.copy(at = wr.transformNodeAfterMoved(wa, lc)))
          case i@operation.Node.Insert(lc, lcs) =>
            reverse(insertMove(i, w))
          case d@operation.Node.Delete(_) =>
            reverse(deleteMove(d, w, conflict.Node.LoserDeletesWinner()))
          case operation.Node.Move(lr, la) =>
            ???
        }
    }
  }
}
