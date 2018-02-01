package shared.data



case class DocumentSnapshot(version: Int, debugRoot: Option[Node])

object DocumentSnapshot {

  private val debugDocument = false
  def apply(v1: Document): DocumentSnapshot = DocumentSnapshot(v1.version,
    if (debugDocument) Some(v1.root) else None)
}
