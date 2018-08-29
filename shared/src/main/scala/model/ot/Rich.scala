package model.ot

import boopickle.Pickler
import model._

import scala.util.Random


object Rich extends Ot[data.Rich, operation.Rich, conflict.Rich] {

  type RebaseResult = Rebased[conflict.Rich, (Seq[operation.Rich], Seq[operation.Rich])]
  override def rebase(winner: operation.Rich, loser: operation.Rich): RebaseResult = {
    val a = ot.EncodedSeq.rebase(winner.u, loser.u)
    val t1 = a.t._1
    val t2 = a.t._2
    val ww = if (t1.isEmpty) Seq.empty else Seq(operation.Rich(t1, winner.ty))
    val ll = if (t2.isEmpty) Seq.empty else Seq(operation.Rich(t2, loser.ty))
    Rebased(a.conflicts, (ww, ll))
  }
}
