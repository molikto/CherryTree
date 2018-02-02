package shared.data

import shared.data.Change.rebaseLine

case class Transaction(transforms: Seq[Change]) {
}


object Transaction {

  def apply(root: Node, changes: Transaction): Node = {
    Change.apply(root, changes.transforms)._1
  }
  def apply(root: Node, changes: Seq[Transaction]): Node = {
    changes.foldLeft(root) { (nodep, c) =>
      Change.apply(nodep, c.transforms)._1
    }
  }
  /**
    * in case the transaction causes non-allowed rebasetype, we stop transforming and return only the ones that allowed
    */
  def rebase(winner: Seq[Transaction], loser: Seq[Transaction], allows: Set[RebaseConflict]): (Seq[Change], Seq[Transaction]) = {
    var earlyReturn = false
    loser.foldLeft(Rebased.Free(winner.flatMap(_.transforms), Seq.empty[Transaction])) { (pair, ll) =>
      pair match {
        case Rebased((wi, lp), t) =>
          if (earlyReturn) {
            pair
          } else {
            val Rebased((wi0, lp0), t0) = Change.rebaseSquare(wi, ll.transforms)
            if (t0.subsetOf(allows)) {
              Rebased((wi0, lp :+ Transaction(lp0)), t ++ t0)
            } else {
              earlyReturn = true
              pair
            }
          }
      }
    }.result
  }
}



