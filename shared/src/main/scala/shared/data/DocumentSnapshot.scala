package shared.data


case class DocumentSnapshot(version: Int)

object DocumentSnapshot {
  def apply(v1: Document): DocumentSnapshot = DocumentSnapshot(v1.version)
}
