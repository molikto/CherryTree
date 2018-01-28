package shared.data



object Authentication {
  case class Input(uid: String)
  case class Token(stuff: String)
}
case class ClientState(
  authentication: Authentication.Token,
  document: Document,
  viewport: Node.Ref)

case class ClientStateUpdate(
  changes: List[Change]
)
