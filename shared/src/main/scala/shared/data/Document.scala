package shared.data

case class Document(version: Int, root: Node)

object Document {
  def empty(id: String) = Document(0, Node.empty(id))
}
