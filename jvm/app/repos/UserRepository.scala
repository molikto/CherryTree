package repos

import java.nio.ByteBuffer
import java.util.UUID

import javax.inject.{Inject, Singleton}
import com.mohiva.play.silhouette.api.{AuthInfo, LoginInfo}
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.impl.providers.{CommonSocialProfile, OAuth1Info, OAuth2Info, OpenIDInfo}
import models.User
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import play.api.libs.json.{Json, Reads, Writes, JsValue, JsObject}


@Singleton
class UserRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
  (implicit ex: ExecutionContext) extends IdentityService[User] with AuthInfoRepository with DatabaseAccessing {

  import utils.MyPostgresProfile.plainApi._


  private implicit val userDbPickler: GetResult[User] = GetResult(r => User(r.<<, r.<<, r.<<, r.<<, r.<<, LoginInfo(r.<<, r.<<)))
  private val AllUserColumns = "user_id, name_, email, avatar_url, activated, provider_id, provider_key"

  private val passwordInfoReads = Json.reads[PasswordInfo]
  private val oauth1InfoReads = Json.reads[OAuth1Info]
  private val oauth2InfoReads = Json.reads[OAuth2Info]
  private val openIdInfoReads = Json.reads[OpenIDInfo]
  private val authInfoReads: Reads[AuthInfo] = (j: JsValue) => {
    val keys = j.asInstanceOf[JsObject].keys
    if (keys.contains("hasher")) {
      passwordInfoReads.reads(j)
    } else if (keys.contains("accessToken")) {
      oauth2InfoReads.reads(j)
    } else if (keys.contains("secret")) {
      oauth1InfoReads.reads(j)
    } else {
      openIdInfoReads.reads(j)
    }
  }
  private val passwordInfoWrites = Json.writes[PasswordInfo]
  private val oauth1InfoWrites = Json.writes[OAuth1Info]
  private val oauth2InfoWrites = Json.writes[OAuth2Info]
  private val openIdInfoWrites = Json.writes[OpenIDInfo]
  private val authInfoWrites: Writes[AuthInfo] = {
    case p: PasswordInfo => passwordInfoWrites.writes(p)
    case p: OAuth1Info => oauth1InfoWrites.writes(p)
    case p: OAuth2Info => oauth2InfoWrites.writes(p)
    case p: OpenIDInfo => openIdInfoWrites.writes(p)
    case _ => throw new IllegalStateException("Not supported")
  }

  private implicit val authInfoGetResult: GetResult[AuthInfo] = {
    val a: GetResult[JsValue] = implicitly
    a.andThen(j => authInfoReads.reads(j).get)
  }

  private implicit val authInfoSet: SetParameter[AuthInfo] = (v1: AuthInfo, v2: PositionedParameters) => {
    val prev: SetParameter[JsValue] = implicitly
    prev(authInfoWrites.writes(v1), v2)
  }


  def retrieve(id: String) : Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where user_id = $id".as[User].headOption)

  def retrieve(loginInfo: LoginInfo): Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where provider_id = ${loginInfo.providerID} and provider_key = ${loginInfo.providerKey}".as[User].headOption)

  def create(u: User, authInfo: AuthInfo): Future[User] = {
    assert(u.userId == "")
    val randomId = UUID.randomUUID().toString
    db.run(
      sqlu"""insert into users values
            ($randomId, ${u.name}, ${u.email}, ${u.avatarUrl.orNull: String}, ${u.activated}, ${u.loginInfo.providerID}, ${u.loginInfo.providerKey}, $authInfo)"""
    ).map(_ => u.copy(userId = randomId))
  }

  def activate(userId: String, activate: Boolean = true): Future[Option[Unit]] =
    db.run(sqlu"update users set activated = $activate where user_id = $userId").map(a => util.positiveOrNoneUnit(a))



  /**
    * this is only used by password authenticator to find a password auth info
    */
  override def find[T <: AuthInfo](loginInfo: LoginInfo)(implicit tag: ClassTag[T]): Future[Option[T]] = {
    db.run(sql"select auth_info from users where provider_id = ${loginInfo.providerID} and provider_key = ${loginInfo.providerKey}".as[AuthInfo].headOption).map(_.asInstanceOf[Option[T]])
  }


  /**
    * this is used when user change/reset password and also hasher needs to be updated
    */
  override def update[T <: AuthInfo](loginInfo: LoginInfo, authInfo: T): Future[T] = {
    val a: AuthInfo = authInfo
    db.run(sqlu"update users set auth_info = $a where provider_id = ${loginInfo.providerID} and provider_key = ${loginInfo.providerKey}").map(_ => authInfo)
  }



  ////





  override def add[T <: AuthInfo](loginInfo: LoginInfo, authInfo: T): Future[T] = throw new IllegalStateException("This is not used")

  override def save[T <: AuthInfo](loginInfo: LoginInfo, authInfo: T): Future[T] = throw new IllegalStateException("This is not used")

  override def remove[T <: AuthInfo](loginInfo: LoginInfo)(implicit tag: ClassTag[T]): Future[Unit] = throw new IllegalStateException("This is not used")
}
