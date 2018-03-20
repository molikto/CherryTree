

         
package shared.data0

import shared.ot._
import scala.util._
import boopickle._


case class Document(root: Node)


object Document {

  type Data = Document

  sealed trait Operation extends OtOperation[Data] {
  }
  object Operation {
    case class Root(child: Node.Operation) extends Operation { override def information: Int = child.information}
  }
  type Transaction = Seq[Operation]

  sealed trait Selection
  object Selection {
    case class Root(child: Node.Selection) extends Selection
  }

  sealed trait Conflict {}
  object Conflict {
    case class Root(child: Node.Conflict) extends Conflict
  }


  object Ot extends shared.ot.Doc[Data, Operation, Conflict, Selection] {


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


    override def apply(op: Operation, sel: Selection): Option[Selection] = {
      (op, sel) match {
        case (Operation.Root(wc), Selection.Root(lc)) => Node.Ot.apply(wc, lc).map(a => Selection.Root(a))
        case _ => Some(sel)
      }
    }



    override def generateRandomData(random: Random) = Document(Node.Ot.generateRandomData(random))

    override def generateRandomChange(data: Data, random: Random): Operation = {
      val i = random.nextInt(1)
      i match {
        case 0 => Operation.Root(Node.Ot.generateRandomChange(data.root, random))
        case _ => throw new IllegalStateException("Not possible")
      }
    }

    override val dataPickler: Pickler[Data] = new Pickler[Data] {
      override def pickle(obj: Data)(implicit state: PickleState): Unit = {
        Node.Ot.dataPickler.pickle(obj.root)
      }
      override def unpickle(implicit state: UnpickleState): Data = {
        Document(Node.Ot.dataPickler.unpickle)
      }
    }

    override val operationPickler: Pickler[Operation] = new Pickler[Operation] {
      override def pickle(obj: Operation)(implicit state: PickleState): Unit = {
        obj match {
          case Operation.Root(child) => state.enc.writeInt(0); Node.Ot.operationPickler.pickle(child)
        }
      }
      override def unpickle(implicit state: UnpickleState): Operation = {
        state.dec.readInt match {
          case 0 => Operation.Root(Node.Ot.operationPickler.unpickle)
        }
      }
    }
  }
}
