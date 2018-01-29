package shared.data

import scala.util.{Random, Try}

object Node extends IdGenerator  {

  // a reference of a child node
  type Ref = Seq[Int] // empty path = root
  object Ref {
    def root = Seq.empty
  }
  type Content = String
  object Content {
    type PointRef = Int
    case class SegmentRef(from: PointRef, to: PointRef)
    def empty = ""
  }

  case class PointRef(child: Node.Ref, contentPoint: Content.PointRef)
  case class SegmentRef(child: Node.Ref, contentSegment: Content.SegmentRef)

  def empty(id: String) = Node(id, "", Seq.empty)
}

case class Node(id: String, content: Node.Content, childs: Seq[Node]) {
  // This might fail
  def map(child: Node.Ref)(p: Node => Node): Node = {
    if (child == Node.Ref.root) {
      p(this)
    } else {
      val index = child.head
      copy(childs = childs.take(index) ++ Seq(childs(index).map(child.tail)(p)) ++ childs.drop(index + 1))
    }
  }
}


sealed class Mode {
}
object Mode {
  case class Normal(segment: Node.SegmentRef) extends Mode
  case class Insert(point: Node.PointRef) extends Mode
  case class Selection(segment: Node.SegmentRef) extends Mode
  case class SelectionTree(node: Node.Ref) extends Mode
}

case class Document(version: Int, root: Node)

object Document {
  def empty = Document(0, Node.empty(Node.newId()))
}

