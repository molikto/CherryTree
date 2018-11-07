package api

case class ConnectionStatus(serverStatus: ServerStatus,
  offline: Boolean = false,
  nodeDeletePending: Boolean = false,
  tempOffline: Boolean = false)

case class ServerStatus(me: Collaborator, online: Seq[Collaborator]) {
}
