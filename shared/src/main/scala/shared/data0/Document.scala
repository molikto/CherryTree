

         
package shared.data0

import shared.ot._


case class Document(root: Node)


object Document {

  type Data = Document

  sealed trait Operation extends OtOperation {
    val child: OtOperation
    override def isDestructive: Boolean = child.isDestructive
  }
  object Operation {
    case class Root(override val child: Node.Operation) extends Operation
  }

  sealed trait Conflict {}
  object Conflict {
    case class Root(child: Node.Conflict) extends Conflict
  }

  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {

    override def apply(c: Operation, data: Data): Data = {
      c match {
        case Operation.Root(child) => data.copy(root = Node.Ot.apply(child, data.root))
      }
    }

    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Option[Operation], Option[Operation])] = {
      (winner, loser) match {
        case (Operation.Root(wc), Operation.Root(lc)) => val c = Node.Ot.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Root(a)), (c.t._1.map(g => Operation.Root(g)), c.t._2.map(g => Operation.Root(g))))
        case _ => Rebased(Set.empty, (Some(winner), Some(loser)))
      }
    }

    override val dataSerializer: Serializer[Data] = _
    override val operationSerializer: Serializer[Operation] = _
  }
}
       