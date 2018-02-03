package shared.data



case class ClientStateSnapshot(
  authentication: Authentication.Token,
  document: DocumentSnapshot
)
object ClientStateSnapshot {
  def apply(authentication: Authentication.Token, state: Document): ClientStateSnapshot = ClientStateSnapshot(
    authentication, DocumentSnapshot(state))
  def apply(clientState: ClientState): ClientStateSnapshot = apply(clientState.authentication, clientState.document)
}
