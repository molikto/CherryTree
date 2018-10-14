package api

import model._

case class ClientInit(session: String, node: data.Node, version: Int, serverStatus: ServerStatus)

