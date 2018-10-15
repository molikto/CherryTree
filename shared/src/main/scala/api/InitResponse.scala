package api

import model._

case class InitResponse(session: String, node: data.Node, version: Int, serverStatus: ServerStatus)

