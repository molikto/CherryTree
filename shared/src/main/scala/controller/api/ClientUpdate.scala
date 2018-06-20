package controller.api

import model._


final case class ClientUpdate(
  winners: Seq[transaction.Node],
  acceptedLosersCount: Int,
  finalVersion: Int
)
