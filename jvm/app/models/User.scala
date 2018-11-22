package models


import java.util.UUID

import api.Collaborator
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.api.{AuthInfo, Identity, LoginInfo}
import server.Server


sealed trait AuthenticationInfo


case class User(
  userId: UUID,
  createdTime: Long,
  name: String,
  email: String,
  avatarUrl: Option[String],
  activated: Boolean,
  loginInfo: LoginInfo
) extends Identity with Server.User {
  override def toCollabrator: Collaborator = Collaborator(email, name, avatarUrl)
}
