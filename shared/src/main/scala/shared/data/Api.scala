package shared.data



object Authentication {
  case class Pass(uid: String)
  case class Token(stuff: String)
}

case class ClientState(
  authentication: Authentication.Token,
  document: Document,
  viewport: Node.Ref)

case class ClientStateUpdate(
  document: DocumentUpdate
)


case class DocumentUpdate(
  version: Int,
  changesBefore: Seq[Change]
)

/**
  * snapshots are state sent from client of current client state,
  * not entire
  */
case class ClientStateSnapshot(
  authentication: Authentication.Token,
  document: DocumentSnapshot
)

case class DocumentSnapshot(version: Int)
