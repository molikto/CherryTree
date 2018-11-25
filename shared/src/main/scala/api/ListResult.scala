package api

import java.util.UUID

case class ListResult(id: UUID, title: model.data.Content, createdTime: Long, updatedTime: Long, owner: Collaborator, permissionLevel: Int)
