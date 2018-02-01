package shared.data



sealed class Mode {
}
object Mode {
  case class Normal(segment: Node.SegmentRef) extends Mode
  case class Insert(point: Node.PointRef) extends Mode
  case class Selection(segment: Node.SegmentRef) extends Mode
  case class SelectionTree(node: Node.Ref) extends Mode
}

