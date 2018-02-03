package shared.data



case class ClientState(
  authentication: Authentication.Token,
  document: Document,
  mode: Mode)

