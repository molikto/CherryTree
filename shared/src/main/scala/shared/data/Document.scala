package shared.data

case class Document(version: Int, root: Node)

object Document {
  def empty = Document(0, Node.empty(Node.newId()))
}
