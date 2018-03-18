

         
package shared.data0

import shared.ot._

import scala.util.Random


case class Node(content: String, childs: Seq[Node])


object Node {

  type Data = Node

  sealed trait Operation extends OtOperation[Data] {
  }
  object Operation {
    case class Content(child: OtStringOperation) extends Operation { override def information: Int = child.information}
    case class Childs(child: SeqOperation[Node, Node.Operation]) extends Operation { override def information: Int = child.information}
  }

  sealed trait Conflict {}
  object Conflict {
    case class Content(child: OtStringConflict) extends Conflict
    case class Childs(child: SeqConflict[Node, Node.Conflict]) extends Conflict
  }

  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {

    override def apply(c: Operation, data: Data): Data = {
      c match {
        case Operation.Content(child) => data.copy(content = OtStringDoc.apply(child, data.content))
        case Operation.Childs(child) => data.copy(childs = Node.Ot.seqOt.apply(child, data.childs))
      }
    }

    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Option[Operation], Option[Operation])] = {
      (winner, loser) match {
        case (Operation.Content(wc), Operation.Content(lc)) => val c = OtStringDoc.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Content(a)), (c.t._1.map(g => Operation.Content(g)), c.t._2.map(g => Operation.Content(g))))
        case (Operation.Childs(wc), Operation.Childs(lc)) => val c = Node.Ot.seqOt.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Childs(a)), (c.t._1.map(g => Operation.Childs(g)), c.t._2.map(g => Operation.Childs(g))))
        case _ => Rebased(Set.empty, (Some(winner), Some(loser)))
      }
    }

    override def generateRandomChange(data: Data, random: Random): Operation = ???

    override def generateRandomData(random: Random): Data = ???
  }
}
       