package repos

import com.mohiva.play.silhouette.api.LoginInfo
import slick.jdbc.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf

trait DBTableDefinitions {

  protected val profile: JdbcProfile
  import profile.api._

  case class UserRow (
    userId: String,
    name: Option[String],
    email: Option[String],
    avatarUrl: Option[String],
    activated: Boolean
  )

  class Users(tag: Tag) extends Table[UserRow](tag, "user") {
    def id = column[String]("user_id", O.PrimaryKey)
    def name = column[Option[String]]("name")
    def email = column[Option[String]]("email")
    def avatarUrl = column[Option[String]]("avatar_url")
    def activated = column[Boolean]("activated")
    def * = (id, name, email, avatarUrl, activated) <> (UserRow.tupled, UserRow.unapply)
  }

  case class LoginInfoRow (
    id: Option[Long],
    providerId: String,
    providerKey: String
  )

  class LoginInfos(tag: Tag) extends Table[LoginInfoRow](tag, "login_info") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def providerId = column[String]("provider_id")
    def providerKey = column[String]("provider_key")
    def * = (id.?, providerId, providerKey) <> (LoginInfoRow.tupled, LoginInfoRow.unapply)
  }

  case class UserLoginInfoRow (
    userId: String,
    loginInfoId: Long
  )

  class UserLoginInfos(tag: Tag) extends Table[UserLoginInfoRow](tag, "user_login_info") {
    def userId = column[String]("user_id")
    def loginInfoId = column[Long]("login_info_id")
    def * = (userId, loginInfoId) <> (UserLoginInfoRow.tupled, UserLoginInfoRow.unapply)
  }

  case class PasswordInfoRow(
    hasher: String,
    password: String,
    salt: Option[String],
    loginInfoId: Long
  )

  class PasswordInfos(tag: Tag) extends Table[PasswordInfoRow](tag, "password_info") {
    def hasher = column[String]("hasher")
    def password = column[String]("password")
    def salt = column[Option[String]]("salt")
    def loginInfoId = column[Long]("login_info_id")
    def * = (hasher, password, salt, loginInfoId) <> (PasswordInfoRow.tupled, PasswordInfoRow.unapply)
  }

  case class OAuth1InfoRow (
    id: Option[Long],
    token: String,
    secret: String,
    loginInfoId: Long
  )

  class OAuth1Infos(tag: Tag) extends Table[OAuth1InfoRow](tag, "oauth1_info") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def token = column[String]("token")
    def secret = column[String]("secret")
    def loginInfoId = column[Long]("login_info_id")
    def * = (id.?, token, secret, loginInfoId) <> (OAuth1InfoRow.tupled, OAuth1InfoRow.unapply)
  }

  case class OAuth2InfoRow (
    id: Option[Long],
    accessToken: String,
    tokenType: Option[String],
    expiresIn: Option[Int],
    refreshToken: Option[String],
    loginInfoId: Long
  )

  class OAuth2Infos(tag: Tag) extends Table[OAuth2InfoRow](tag, "oauth2_info") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def accessToken = column[String]("access_token")
    def tokenType = column[Option[String]]("token_type")
    def expiresIn = column[Option[Int]]("expires_in")
    def refreshToken = column[Option[String]]("refresh_token")
    def loginInfoId = column[Long]("login_info_id")
    def * = (id.?, accessToken, tokenType, expiresIn, refreshToken, loginInfoId) <> (OAuth2InfoRow.tupled, OAuth2InfoRow.unapply)
  }

  case class OpenIDInfoRow (
    id: String,
    loginInfoId: Long,
    attributes: Array[Byte]
  )

  class OpenIDInfos(tag: Tag) extends Table[OpenIDInfoRow](tag, "openid_info") {
    def id = column[String]("id", O.PrimaryKey)
    def loginInfoId = column[Long]("login_info_id")
    def attributes = column[Array[Byte]]("attributes")
    def * = (id, loginInfoId, attributes) <> (OpenIDInfoRow.tupled, OpenIDInfoRow.unapply)
  }

  // table query definitions
  val Users = TableQuery[Users]
  val LoginInfos = TableQuery[LoginInfos]
  val UserLoginInfos = TableQuery[UserLoginInfos]
  val PasswordInfos = TableQuery[PasswordInfos]
  val OAuth1Infos = TableQuery[OAuth1Infos]
  val OAuth2Infos = TableQuery[OAuth2Infos]
  val OpenIDInfos = TableQuery[OpenIDInfos]

  // queries used in multiple places
  def loginInfoQuery(loginInfo: LoginInfo) =
    LoginInfos.filter(dbLoginInfo => dbLoginInfo.providerId === loginInfo.providerID && dbLoginInfo.providerKey === loginInfo.providerKey)
}