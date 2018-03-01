

         
package shared.data0

import shared.ot._


case class Node(content: Content, childs: Seq[Node])


object Node {

  type Data = Node

  sealed trait Operation extends OtOperation {
    val child: OtOperation
    override def isDestructive: Boolean = child.isDestructive
  }
  object Operation {
    case class Content(override val child: Content.Operation) extends Operation
    case class Childs(override val child: SeqOperation[Node.Operation]) extends Operation
  }

  sealed trait Conflict {}
  object Conflict {
    case class Content(child: Content.Conflict) extends Conflict
    case class Childs(child: SeqConflict[Node.Conflict]) extends Conflict
  }

  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {

    override def apply(c: Operation, data: Data): Data = {
      c match {
        case Operation.Content(child) => data.copy(content = Content.Ot.apply(child, data.content))
        case Operation.Childs(child) => data.copy(childs = Node.Ot.seqOt.apply(child, data.childs))
      }
    }

    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Option[Operation], Option[Operation])] = {
      (winner, loser) match {
        case (Operation.Content(wc), Operation.Content(lc)) => val c = Content.Ot.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Content(a)), (c.t._1.map(g => Operation.Content(g)), c.t._2.map(g => Operation.Content(g))))
        case (Operation.Childs(wc), Operation.Childs(lc)) => val c = Node.Ot.seqOt.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Childs(a)), (c.t._1.map(g => Operation.Childs(g)), c.t._2.map(g => Operation.Childs(g))))
        case _ => Rebased(Set.empty, (Some(winner), Some(loser)))
      }
    }

    override val dataSerializer: Serializer[Data] = _
    override val operationSerializer: Serializer[Operation] = _
  }
}
       