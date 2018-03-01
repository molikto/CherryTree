

         
package shared.data0

import shared.ot._


case class KeySetting(name: String)


object KeySetting {

  type Data = KeySetting

  sealed trait Operation extends OtOperation[Data] {
  }
  object Operation {
    case class Name(child: StringOperation) extends Operation { override def isDestructive: Boolean = child.isDestructive}
  }

  sealed trait Conflict {}
  object Conflict {
    case class Name(child: StringConflict) extends Conflict
  }

  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {

    override def apply(c: Operation, data: Data): Data = {
      c match {
        case Operation.Name(child) => data.copy(name = StringOt.apply(child, data.name))
      }
    }

    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Option[Operation], Option[Operation])] = {
      (winner, loser) match {
        case (Operation.Name(wc), Operation.Name(lc)) => val c = StringOt.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Name(a)), (c.t._1.map(g => Operation.Name(g)), c.t._2.map(g => Operation.Name(g))))
        case _ => Rebased(Set.empty, (Some(winner), Some(loser)))
      }
    }

    override val dataSerializer: Serializer[Data] = _
    override val operationSerializer: Serializer[Operation] = _
  }
}
       