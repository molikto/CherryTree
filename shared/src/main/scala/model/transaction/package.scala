package model

package object transaction {



  type Node = Seq[operation.Node]

  object Node {

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
  }
}
