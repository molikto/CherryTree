package shared.data

object Node {
  // a reference of a child node
  type Ref = List[Int] // empty path = root
  object Ref {
    def root = List.empty
  }
  type Content = String
  object Content {
    type PointRef = Int
    case class SegmentRef(from: PointRef, to: PointRef)
  }

  case class PointRef(child: Node.Ref, contentPoint: Content.PointRef)
  case class SegmentRef(child: Node.Ref, contentSegment: Content.SegmentRef)
}

case class Node(id: String, content: Node.Content, childs: List[Node])



sealed class Mode {
}
object Mode {
  case class Normal(segment: Node.SegmentRef) extends Mode
  case class Insert(point: Node.PointRef)
  case class Selection(segment: Node.SegmentRef) extends Mode
  case class SelectionTree(node: Node.Ref) extends Mode
}

case class Document(root: Node)

object Document {
  def empty = Document(Node("0", "What", List.empty))
}

