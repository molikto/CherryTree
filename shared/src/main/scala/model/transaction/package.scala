package model

package object transaction {



  type Node = Seq[operation.Node]

  object Node {
    def reverse(before: data.Node, trans: Node): Node = {
      if (trans.isEmpty) {
        trans
      } else if (trans.size == 1) {
        Seq(trans.head.reverse(before))
      } else {
        trans.foldLeft((before, Seq.empty[operation.Node])) { (s, a) =>
          val op = a.reverse(s._1)
          (a(s._1), op +: s._2)
        }._2
      }
    }

    def transform(pp: Node, m: Option[mode.Node]): Option[mode.Node] = {
      pp.foldLeft(m) {(m, p) => p.transform(m) }
    }

    def transformSeq(pp: Seq[Node], m: Option[mode.Node]): Option[mode.Node] = {
      transform(pp.flatten, m)
    }

    def mergeForUndoer(current: Node, before: Node): Option[Node] = {
      (current, before) match {
        case (Seq(c), Seq(b)) =>
          c.mergeForUndoer(b) match {
            case Some(a) =>
              Some(Seq(a))
            case _ => None
          }
        case _ => None
      }
    }
  }
}
