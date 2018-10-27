package models


import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.api.{AuthInfo, Identity, LoginInfo}


sealed trait AuthenticationInfo


case class User(
  userId: String,
  name: String,
  email: String,
  avatarUrl: Option[String],
  activated: Boolean,
  loginInfo: LoginInfo
) extends Identity {
}
