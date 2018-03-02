

         
package shared.data0

import shared.ot._


case class Settings(keys: Set[KeySetting])


object Settings {

  type Data = Settings

  sealed trait Operation extends OtOperation[Data] {
  }
  object Operation {
    case class Keys(child: SetOperation[KeySetting.Operation]) extends Operation { override def information: Int = child.information}
  }

  sealed trait Conflict {}
  object Conflict {
    case class Keys(child: SetConflict[KeySetting.Conflict]) extends Conflict
  }

  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {

    override def apply(c: Operation, data: Data): Data = {
      c match {
        case Operation.Keys(child) => data.copy(keys = KeySetting.Ot.setOt.apply(child, data.keys))
      }
    }

    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Option[Operation], Option[Operation])] = {
      (winner, loser) match {
        case (Operation.Keys(wc), Operation.Keys(lc)) => val c = KeySetting.Ot.setOt.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.Keys(a)), (c.t._1.map(g => Operation.Keys(g)), c.t._2.map(g => Operation.Keys(g))))
        case _ => Rebased(Set.empty, (Some(winner), Some(loser)))
      }
    }

    override val dataSerializer: Serializer[Data] = _
    override val operationSerializer: Serializer[Operation] = _
  }
}
       