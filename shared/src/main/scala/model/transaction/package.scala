package model

import scala.collection.mutable.ArrayBuffer

package object transaction {



  type Node = Seq[operation.Node]

  object Node {

    def mergeSingleOpTransactions(a: Seq[Node], whitespace: Boolean = false): Seq[Node] = {
      if (a.size <= 1) {
        a
      } else {
        val bf = new ArrayBuffer[Node]()
        bf.append(a.head)
        for (t <- a.tail) {
          mergeSingleOpTransactions(t, bf.last, whitespace) match {
            case Some(o) =>
              bf.remove(bf.size - 1)
              bf.append(o)
            case None =>
              bf.append(t)
          }
        }
        bf
      }
    }

    def mergeSingleOpTransactions(current: Node, before: Node, whitespace: Boolean): Option[Node] = {
      (current, before) match {
        case (Seq(c), Seq(b)) =>
          c.merge(b, whitespace) match {
            case Some(a) =>
              Some(Seq(a))
            case _ => None
          }
        case _ => None
      }
    }

    val pickler: Pickler[Node] =new Pickler[Node] {
      override def pickle(obj: Node)(implicit state: PickleState): Unit = {
        import state.enc._
        writeInt(obj.size)
        for (o <- obj) operation.Node.jsonFormat.pickle(o)
      }

      override def unpickle(implicit state: UnpickleState): Node = {
        import state.dec._
        (0 until readInt).map(_ => operation.Node.jsonFormat.unpickle)
      }
    }

  }
}
