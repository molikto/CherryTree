package models


import java.util.UUID

import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.api.{AuthInfo, Identity, LoginInfo}


sealed trait AuthenticationInfo


case class User(
  userId: UUID,
  createdTime: Long,
  name: String,
  email: String,
  avatarUrl: Option[String],
  activated: Boolean,
  loginInfo: LoginInfo
) extends Identity {
}
