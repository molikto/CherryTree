package model.ot

import boopickle.Pickler
import model._

import scala.util.Random

object Content extends Ot[data.Content, operation.Content, conflict.Content] {

  type RebaseResult = Rebased[conflict.Content, (Option[operation.Content], Option[operation.Content])]
  override def rebase(winner: operation.Content, loser: operation.Content): RebaseResult = {
    winner match  {
      case operation.Content.Code.Content(w) =>
        loser match  {
          case operation.Content.Code.Content(l) =>
            val uc = ot.Unicode.rebase(w, l)
            Rebased(uc.conflicts.map(conflict.Content.Code.Content),
              (uc.t._1.map(operation.Content.Code.Content), uc.t._2.map(operation.Content.Code.Content)))
          case operation.Content.Code.Lang(l) =>
            Rebased(Set.empty, (Some(winner), Some(loser)))
          case operation.Content.Paragraph.Content(l) =>
            throw new AssertionError()
        }
      case operation.Content.Code.Lang(w) =>
        loser match {
          case operation.Content.Code.Content(l) =>
            Rebased(Set.empty, (Some(winner), Some(loser)))
          case operation.Content.Code.Lang(l) =>
            if (w == l) {
              Rebased(Set.empty, (None, None))
            } else {
              Rebased(Set(conflict.Content.Code.Lang(w)), (None, Some(winner)))
            }
          case operation.Content.Paragraph.Content(l) =>
            throw new AssertionError()
        }
      case operation.Content.Paragraph.Content(w) =>
        loser match  {
          case operation.Content.Paragraph.Content(l) =>
            val uc = ot.Paragraph.rebase(w, l)
            Rebased(uc.conflicts.map(conflict.Content.Code.Content),
              (uc.t._1.map(a => operation.Content.Paragraph.Content(a)), uc.t._2.map(a => operation.Content.Paragraph.Content(a))))
          case _ =>
            throw new AssertionError()
        }
    }
  }

  override def generateRandomChange(d: data.Content, random: Random): operation.Content = ???

  override def generateRandomData(random: Random): data.Content = ???

  override val dataPickler: Pickler[data.Content] = data.Content.pickler
  override val operationPickler: Pickler[operation.Content] = operation.Content.pickler
}
