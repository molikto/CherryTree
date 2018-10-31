package repos

import java.nio.ByteBuffer
import java.util.UUID

import api.PermissionLevel
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
import play.api.libs.json.{Format, JsObject, JsResult, JsValue, Json}
import model._
import api._


@Singleton
class UserRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
  (implicit ex: ExecutionContext) extends IdentityService[User] with AuthInfoRepository with DatabaseAccessing {


  import utils.MyPostgresProfile.plainApi._


  private implicit val userDbPickler: GetResult[User] = GetResult(r => User(r.<<, r.<<, r.<<, r.<<, r.<<, LoginInfo(r.<<, r.<<)))
  private val AllUserColumns = "user_id, name_, email, avatar_url, activated, provider_id, provider_key"

  implicit val passwordInfoFormat = Json.format[PasswordInfo]
  implicit val oauth1InfoFormat = Json.format[OAuth1Info]
  implicit val oauth2InfoFormat = Json.format[OAuth2Info]
  implicit val openIdInfoFormat = Json.format[OpenIDInfo]
  private val authInfoFormat: Format[AuthInfo] = new Format[AuthInfo] {
    override def reads(j: JsValue): JsResult[AuthInfo] = {
      val keys = j.asInstanceOf[JsObject].keys
      if (keys.contains("hasher")) {
        passwordInfoFormat.reads(j)
      } else if (keys.contains("accessToken")) {
        oauth2InfoFormat.reads(j)
      } else if (keys.contains("secret")) {
        oauth1InfoFormat.reads(j)
      } else {
        openIdInfoFormat.reads(j)
      }
    }

    override def writes(o: AuthInfo): JsValue = o match {
      case p: PasswordInfo => passwordInfoFormat.writes(p)
      case p: OAuth1Info => oauth1InfoFormat.writes(p)
      case p: OAuth2Info => oauth2InfoFormat.writes(p)
      case p: OpenIDInfo => openIdInfoFormat.writes(p)
      case _ => throw new IllegalStateException("Not supported")
    }
  }

  private implicit val authInfoGetResult: GetResult[AuthInfo] = {
    val a: GetResult[JsValue] = implicitly
    a.andThen(j => authInfoFormat.reads(j).get)
  }

  private implicit val authInfoSet: SetParameter[AuthInfo] = (v1: AuthInfo, v2: PositionedParameters) => {
    val prev: SetParameter[JsValue] = implicitly
    prev(authInfoFormat.writes(v1), v2)
  }


  def retrieve(id: String) : Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where user_id = $id".as[User].headOption)

  def retrieve(loginInfo: LoginInfo): Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where provider_id = ${loginInfo.providerID} and provider_key = ${loginInfo.providerKey}".as[User].headOption)

  def create(u: User, authInfo: AuthInfo, documentTitle: String): Future[User] = {
    assert(u.userId == "")
    val userId = UUID.randomUUID().toString
    val createUser =
      sqlu"insert into users values ($userId, ${u.name}, ${u.email}, ${u.avatarUrl.orNull: String}, ${u.activated}, ${u.loginInfo.providerID}, ${u.loginInfo.providerKey}, $authInfo)"
    val documentId = UUID.randomUUID().toString
    val node = model.data.Node.create(documentTitle)
    val createDefaultDocument =
      sqlu"insert into documents values ($documentId, ${node.uuid}, 0)"
    val createPermission =
      sqlu"insert into permissions values ($userId, $documentId, ${PermissionLevel.Admin})"
    val createInitDocumentNodes =
      sqlu"insert into nodes values ($documentId, ${node.uuid}, ${0}, ${Long.MaxValue}, ${Seq.empty[String]}, ${model.toArray(node.attributes)}, ${model.toArray(node.content)})"
    db.run(DBIO.seq(createUser, createDefaultDocument, createPermission, createInitDocumentNodes).transactionally).map(_ => u.copy(userId = userId))
  }

  def indexDocumentId(userId: String): Future[Option[String]] = {
    db.run(sql"select document_id from permissions where user_id = $userId and permission_level = ${PermissionLevel.Admin}".as[String].headOption)
  }

  def hasReadPermission(userId: String, documentId: String): _root_.scala.concurrent.Future[Boolean] = {
    db.run(sql"select document_id from permissions where document_id = $documentId and user_id = $userId and permission_level >= ${PermissionLevel.ReadOnly}".as[String].headOption).map(_.isDefined)
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
