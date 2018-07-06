package model.ot


import boopickle.Pickler
import model._
import model.operation.Node
import com.softwaremill.quicklens._

import scala.util.Random

object Node extends Ot[data.Node, operation.Node, conflict.Node] {

  type RebaseResult = Rebased[conflict.Node, (Option[operation.Node], Option[operation.Node])]

  // LATER handle move
  override def rebase(winner: operation.Node, loser: operation.Node): RebaseResult = {
    def insertDelete(i: operation.Node.Insert, d: operation.Node.Delete, deleteConflict : => conflict.Node): RebaseResult = {
      if (d.r.start == i.c) { // a special case that the insertion is still valid after the deletion
        free(i, d.modify(_.r).using(_.modify(_.childs).using(_.moveBy(i.childs.size))))
      } else { // in other cases, deleting the insertion point is deleting the entire insertion
        d.r.transformAfterDeleted(i.c) match {
          case Some(p) =>
            free(
              i.copy(c = p),
              d.modify(_.r).using(r => range.Node(cursor.Node.transformAfterInserted(i.c, i.childs.size, r.start), r.childs.size))
              )
          case None => Rebased(Set(deleteConflict), (None, Some(d.modify(_.r).using(r => range.Node(r.start, r.childs.size + i.childs.size)))))
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
              Rebased(Set(conflict.Node.ReplacedByLoser()), (Some(loser), None))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(w.copy(c = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), loser)
          case operation.Node.Delete(lr) =>
            lr.transformAfterDeleted(wc) match {
              case Some(p) => free(w.copy(c = p), loser)
              case None => Rebased(Set(conflict.Node.LoserDeletesWinner()), (None, Some(loser)))
            }
          case operation.Node.Move(lr, la) =>
            ???
        }
      case w@operation.Node.Replace(wc, _) =>
        loser match {
          case operation.Node.Content(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.ReplacedByWinner()), (Some(winner), None))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Some(winner), None))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(w.copy(c = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), loser)
          case operation.Node.Delete(lr) =>
            lr.transformAfterDeleted(wc) match {
              case Some(p) => free(w.copy(c = p), loser)
              case None => Rebased(Set(conflict.Node.LoserDeletesWinner()), (None, Some(loser)))
            }
          case operation.Node.Move(lr, la) =>
            ???
        }
      case w@operation.Node.Insert(wc, wcs) =>
        loser match {
          case l@operation.Node.Content(lc, _) =>
            free(w, l.copy(c = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Replace(lc, _) =>
            free(w, l.copy(c = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Insert(lc, lcs) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.Asymmetry()), some(w, l.copy(c = cursor.Node.transformAfterInserted(wc, wcs.size, wc))))
            } else {
              free(w.copy(c = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), l.copy(c = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
            }
          case d@operation.Node.Delete(_) =>
            insertDelete(w, d, conflict.Node.LoserDeletesWinner())
          case operation.Node.Move(lr, la) =>
            ???
        }
      case d@operation.Node.Delete(wr) =>
        loser match {
          case l@operation.Node.Content(lc, _) =>
            wr.transformAfterDeleted(lc) match {
              case Some(p) => free(winner, l.copy(c = p))
              case None => Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Some(winner), None))
            }
          case l@operation.Node.Replace(lc, _) =>
            wr.transformAfterDeleted(lc) match {
              case Some(p) => free(winner, l.copy(c = p))
              case None => Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Some(winner), None))
            }
          case i@operation.Node.Insert(_, _) =>
            reverse(insertDelete(i, d, conflict.Node.WinnerDeletesLoser()))
          case operation.Node.Delete(lr) =>
            val wp = lr.transformDeletingRangeAfterDeleted(wr).map(operation.Node.Delete)
            val lp = wr.transformDeletingRangeAfterDeleted(lr).map(operation.Node.Delete)
            Rebased(Set.empty, (wp, lp))
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Move(wr, wa) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            ???
          case operation.Node.Replace(lc, lo) =>
            ???
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
    }
  }
}
