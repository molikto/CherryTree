package model.ot

import boopickle.Pickler
import model._

import scala.util.Random


object Paragraph extends Ot[data.Paragraph, operation.Paragraph, conflict.Paragraph] {

  type RebaseResult = Rebased[conflict.Paragraph, (Seq[operation.Paragraph], Seq[operation.Paragraph])]
  override def rebase(winner: operation.Paragraph, loser: operation.Paragraph): RebaseResult = {
    val a = ot.Unicode.rebase(winner.u, loser.u)
    val t1 = a.t._1
    val t2 = a.t._2
    val ww = if (t1.isEmpty) Seq.empty else Seq(operation.Paragraph(t1, winner.ty))
    val ll = if (t2.isEmpty) Seq.empty else Seq(operation.Paragraph(t2, loser.ty))
    Rebased(a.conflicts, (ww, ll))
  }
}
