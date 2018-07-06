package model.ot

import boopickle.Pickler
import model._

import scala.util.Random


object Paragraph extends Ot[data.Paragraph, operation.Paragraph, conflict.Paragraph] {

  type RebaseResult = Rebased[conflict.Paragraph, (Option[operation.Paragraph], Option[operation.Paragraph])]
  override def rebase(winner: operation.Paragraph, loser: operation.Paragraph): RebaseResult = {
    val a = ot.Unicode.rebase(winner.u, loser.u)
  //  Rebased(a.conflicts, (a.t._1.map(a => operation.Paragraph(a)), a.t._2.map(b => operation.Paragraph(b))))
    ???
  }
}
