package models

import java.util.UUID

import com.mohiva.play.silhouette.api.{ Identity, LoginInfo }

case class User(
  userID: String,
  loginInfo: LoginInfo,
  name: Option[String],
  email: Option[String],
  avatarURL: Option[String],
  activated: Boolean) extends Identity {
}
