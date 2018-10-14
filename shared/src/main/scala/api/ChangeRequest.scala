package api

import model.transaction

final case class ChangeRequest(
  session: String,
  clientVersion: Int,
  ts: Seq[transaction.Node],
  mode: Option[model.mode.Node],
  debugClientDoc: Int
)
