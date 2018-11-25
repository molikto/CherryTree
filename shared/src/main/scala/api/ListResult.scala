package api

import java.util.UUID

case class ListResult(id: UUID, title: model.data.Content, createdTime: Long, updatedTime: Long, permissionLevel: Int)
