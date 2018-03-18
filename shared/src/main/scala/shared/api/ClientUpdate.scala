package shared.api

import shared.data0.Node


final case class ClientUpdate(
  winners: Seq[Node.Transaction],
  acceptedLosersCount: Int
)
