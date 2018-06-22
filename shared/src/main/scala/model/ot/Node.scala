package model.ot


import boopickle.Pickler
import model._

import scala.util.Random

object Node extends Ot[data.Node, operation.Node, conflict.Node] {

  type RebaseResult = Rebased[conflict.Unicode, (Option[operation.Unicode], Option[operation.Unicode])]


  override def rebase(winner: operation.Node, loser: operation.Node): Rebased[conflict.Node, (Option[operation.Node], Option[operation.Node])] = {
    winner match {
      case operation.Node.Content(wc, wo) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            if (wc == lc) {
              val r = Content.rebase(wo, lo)
              Rebased(r.conflicts.map(c => conflict.Node.Content(c)), map[operation.Content, operation.Node](r.t, a => operation.Node.Content(wc, a)))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, lo) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.ReplacedByLoser()), (Some(loser), None))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(
              operation.Node.Content(cursor.Node.transformInsertionPointAfterInserted(lc, lcs.size, wc), wo),
              loser)
          case operation.Node.Delete(lr) =>
            if (lr.containsCursor(wc)) {
              Rebased(Set(conflict.Node.LoserDeletesWinner()), (None, Some(loser)))
            } else {
              free(
                operation.Node.Content(lr.transformCursorAfterDeleted(wc), wo),
                loser)
            }
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Replace(wc, wo) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Some(winner), None))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, lo) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Some(winner), None))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Insert(wc, wcs) =>
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
      case operation.Node.Delete(wr) =>
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

  override def generateRandomChange(MODEL: data.Node, random: Random): operation.Node = ???

  override def generateRandomData(random: Random): data.Node = ???

  override val dataPickler: Pickler[data.Node] = data.Node.pickler
  override val operationPickler: Pickler[operation.Node] = operation.Node.pickler
}
