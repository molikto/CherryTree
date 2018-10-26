package models


import com.mohiva.play.silhouette.api.{ Identity, LoginInfo }

case class User(
  userId: String,
  loginInfo: LoginInfo,
  name: Option[String],
  email: Option[String],
  avatarUrl: Option[String],
  activated: Boolean) extends Identity {
}
