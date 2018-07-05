package model.ot


import boopickle.Pickler
import model._

import scala.util.Random

object Node extends Ot[data.Node, operation.Node, conflict.Node] {

  type RebaseResult = Rebased[conflict.Unicode, (Option[operation.Unicode], Option[operation.Unicode])]


  override def rebase(winner: operation.Node, loser: operation.Node): Rebased[conflict.Node, (Option[operation.Node], Option[operation.Node])] = {
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
            free(winner, l.copy(c = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Replace(lc, _) =>
            free(winner, l.copy(c = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case operation.Node.Insert(lc, lcs) =>
            ??? // TODO
          case operation.Node.Delete(lr) =>
            ??? // TODO
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Delete(wr) =>
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
          case operation.Node.Insert(lc, lcs) =>
            ??? // TODO
          case operation.Node.Delete(lr) =>
            ??? // TODO
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
