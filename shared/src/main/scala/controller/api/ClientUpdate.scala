package controller.api

import shared._


final case class ClientUpdate(
  winners: Seq[transaction.Node],
  acceptedLosersCount: Int,
  finalVersion: Int
)
