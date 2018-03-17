package shared

import shared.ot.{AtomicOt, OtStringOperation, SeqOperation}

package object data0 {



  object NodeOps {

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
