package model.ot

import boopickle.Pickler
import model._

import scala.util.Random

object Content extends Ot[data.Content, mode.Content, operation.Content, conflict.Content] {

  type RebaseResult = Rebased[conflict.Content, (Seq[operation.Content], Seq[operation.Content])]
  override def rebase(winner: operation.Content, loser: operation.Content): RebaseResult = {
    winner match  {
      case operation.Content.CodeContent(w) =>
        loser match  {
          case operation.Content.CodeContent(l) =>
            val uc = ot.Unicode.rebase(w, l)
            Rebased(uc.conflicts.map(conflict.Content.CodeContent),
              (uc.t._1.map(operation.Content.CodeContent), uc.t._2.map(operation.Content.CodeContent)))
          case operation.Content.CodeLang(l) =>
            free(winner, loser)
          case operation.Content.Rich(l) =>
            throw new IllegalStateException("Not applicable operation")
        }
      case operation.Content.CodeLang(w) =>
        loser match {
          case operation.Content.CodeContent(l) =>
            free(winner, loser)
          case operation.Content.CodeLang(l) =>
            if (w == l) {
              Rebased(Set.empty, (Seq.empty, Seq.empty))
            } else {
              Rebased(Set(conflict.Content.CodeLang(w)), (Seq(winner), Seq.empty))
            }
          case operation.Content.Rich(l) =>
            throw new IllegalStateException("Not applicable operation")
        }
      case operation.Content.Rich(w) =>
        loser match  {
          case operation.Content.Rich(l) =>
            val uc = ot.Rich.rebase(w, l)
            Rebased(uc.conflicts.map(conflict.Content.CodeContent),
              (uc.t._1.map(a => operation.Content.Rich(a)), uc.t._2.map(a => operation.Content.Rich(a))))
          case _ =>
            throw new IllegalStateException("Not applicable operation")
        }
    }
  }
}
