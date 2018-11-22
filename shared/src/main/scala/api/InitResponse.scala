package api

import model._

case class InitResponse(
  node: data.Node,
  version: Int,
  permissionLevel: Int,
  serverStatus: ServerStatus)

