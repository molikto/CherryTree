package shared.data


case class ClientStateSnapshot(
  authentication: Authentication.Token,
  document: DocumentSnapshot
)
object ClientStateSnapshot {
  def apply(state: ClientState): ClientStateSnapshot = ClientStateSnapshot(state.authentication, DocumentSnapshot(state.document))
}
