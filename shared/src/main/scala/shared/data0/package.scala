package shared

import shared.ot._

package object data0 extends Picklers {

  object NodeOps {


    def isAsymmetry(n: SeqConflict[Node, Node.Conflict]): Boolean = n match {
      case SeqConflict.Asymmetry() => true
      case SeqConflict.Child(k) => isAsymmetry(k)
      case _ => false
    }

    def isAsymmetry(a: Node.Conflict): Boolean = a match {
      case Node.Conflict.Content(c) => c match {
        case OtStringConflict.Asymmetry() => true
        case _ => false
      }
      case Node.Conflict.Childs(n) => isAsymmetry(n)
    }

    def isAsymmetry(op: Set[Node.Conflict]): Boolean = op.exists(a => isAsymmetry(a))


    def insertNode(at: Seq[Int], content: String): Node.Operation = {
      if (at.isEmpty) {
        throw new IllegalArgumentException("")
      } else if (at.length == 1) {
        Node.Operation.Childs(SeqOperation.Add(at.head, Seq(Node(content, Seq.empty))))
      } else {
         Node.Operation.Childs(SeqOperation.Child(at.head, insertNode(at.tail, content)))
      }
    }

    def insertContent(at: Seq[Int], p: Int, content: String): Node.Operation = {
      if (at.isEmpty) {
        Node.Operation.Content(OtStringOperation.Add(p, content))
      } else {
        Node.Operation.Childs(SeqOperation.Child(at.head, insertContent(at.tail, p, content)))
      }
    }

    def deleteContent(at: Seq[Int], from: Int, len: Int): Node.Operation = {
      if (at.isEmpty) {
        Node.Operation.Content(OtStringOperation.Delete(from, from + len))
      } else {
        Node.Operation.Childs(SeqOperation.Child(at.head, deleteContent(at.tail, from, from + len - 1)))
      }
    }

    def deleteNode(at: Seq[Int]): Node.Operation = {
      if (at.isEmpty) {
        throw new IllegalArgumentException("")
      } else if (at.length == 1) {
        Node.Operation.Childs(SeqOperation.Delete(at.head, at.head + 1))
      } else {
        Node.Operation.Childs(SeqOperation.Child(at.head, deleteNode(at.tail)))
      }
    }
  }
}
