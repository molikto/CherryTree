package api

case class ConnectionStatus(serverStatus: ServerStatus,
  offline: Boolean = false,
  nodeDeletePending: Boolean = false,
  tempOffline: Set[String] = Set.empty)

case class ServerStatus(me: Collaborator, online: Seq[Collaborator]) {
}
