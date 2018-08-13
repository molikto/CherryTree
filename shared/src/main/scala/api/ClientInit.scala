package api

import model._

case class ClientInit(node: data.Node, version: Int, serverStatus: ServerStatus)

