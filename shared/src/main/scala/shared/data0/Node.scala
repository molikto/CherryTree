

         
package shared.data0

import shared.ot._
import scala.util._
import boopickle._


case class Node(content: String, childs: Seq[Node])


object Node {

  type Data = Node

  sealed trait Operation extends OtOperation[Data] {
  }
  object Operation {
    case class Content(child: OtStringOperation) extends Operation { override def information: Int = child.information}
    case class Childs(child: SeqOperation[Node, Node.Operation]) extends Operation { override def information: Int = child.information}
  }
  type Transaction = Seq[Operation]

  sealed trait Selection
  object Selection {
    case class Content(child: OtStringSelection) extends Selection
    case class Childs(child: SeqSelection[Node.Selection]) extends Selection
  }

  sealed trait Conflict {}
  object Conflict {
    case class Content(child: OtStringConflict) extends Conflict
    case class Childs(child: SeqConflict[Node, Node.Conflict]) extends Conflict
  }


  object Ot extends shared.ot.Doc[Data, Operation, Conflict, Selection] {


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


    override def apply(op: Operation, sel: Selection): Option[Selection] = {
      (op, sel) match {
        case (Operation.Content(wc), Selection.Content(lc)) => OtStringDoc.apply(wc, lc).map(a => Selection.Content(a))
        case (Operation.Childs(wc), Selection.Childs(lc)) => Node.Ot.seqOt.apply(wc, lc).map(a => Selection.Childs(a))
        case _ => Some(sel)
      }
    }



    override def generateRandomModel(random: Random) = Node(OtStringDoc.generateRandomModel(random), Node.Ot.seqOt.generateRandomModel(random))

    override def generateRandomChange(data: Data, random: Random): Operation = {
      val i = random.nextInt(2)
      i match {
        case 0 => Operation.Content(OtStringDoc.generateRandomChange(data.content, random))
        case 1 => Operation.Childs(Node.Ot.seqOt.generateRandomChange(data.childs, random))
        case _ => throw new IllegalStateException("Not possible")
      }
    }

    override val dataPickler: Pickler[Data] = new Pickler[Data] {
      override def pickle(obj: Data)(implicit state: PickleState): Unit = {
        OtStringDoc.dataPickler.pickle(obj.content)
        Node.Ot.seqOt.dataPickler.pickle(obj.childs)
      }
      override def unpickle(implicit state: UnpickleState): Data = {
        Node(OtStringDoc.dataPickler.unpickle(state), Node.Ot.seqOt.dataPickler.unpickle(state))
      }
    }

    override val operationPickler: Pickler[Operation] = new Pickler[Operation] {
      override def pickle(obj: Operation)(implicit state: PickleState): Unit = {
        obj match {
          case Operation.Content(child) => state.enc.writeInt(0); OtStringDoc.operationPickler.pickle(child)
          case Operation.Childs(child) => state.enc.writeInt(1); Node.Ot.seqOt.operationPickler.pickle(child)
        }
      }
      override def unpickle(implicit state: UnpickleState): Operation = {
        state.dec.readInt match {
          case 0 => Operation.Content(OtStringDoc.operationPickler.unpickle(state))
          case 1 => Operation.Childs(Node.Ot.seqOt.operationPickler.unpickle(state))
        }
      }
    }
  }
}
