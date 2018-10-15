package api

import model._


final case class ChangeResponse(
  winners: Seq[transaction.Node],
  acceptedLosersCount: Int,
  finalVersion: Int,
  serverStatus: ServerStatus
)
