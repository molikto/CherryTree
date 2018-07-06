package model.ot

import boopickle.Pickler
import model._

import scala.util.Random


object Paragraph extends Ot[data.Paragraph, operation.Paragraph, conflict.Paragraph] {

  type RebaseResult = Rebased[conflict.Paragraph, (Option[operation.Paragraph], Option[operation.Paragraph])]
  override def rebase(winner: operation.Paragraph, loser: operation.Paragraph): RebaseResult = {
    val a = ot.Unicode.rebase(winner.u, loser.u)
    // TODO what's the purpose of ty here? and how is it used?
    val ww = if (a.t._1.isEmpty) None else Some(operation.Paragraph(a.t._1, winner.ty))
    val ll = if (a.t._2.isEmpty) None else Some(operation.Paragraph(a.t._2, loser.ty))
    Rebased(a.conflicts, (ww, ll))
  }
}
