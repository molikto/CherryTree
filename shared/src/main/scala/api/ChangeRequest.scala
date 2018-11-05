package api

import java.util.UUID

import model.transaction

final case class ChangeRequest(
  clientVersion: Int,
  ts: Seq[(transaction.Node, UUID)],
  mode: Option[model.mode.Node],
  debugClientDoc: Int
)
