package shared.data

case class Document(version: Int, root: Node) {

  def initialMode: Mode = Mode.Normal(Node.SegmentRef(Node.Ref.root, Node.Content.SegmentRef(0, 1)))
}

object Document {
  def empty(id: String) = Document(0, Node.empty(id))

}
