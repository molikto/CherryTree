package repos

import java.nio.ByteBuffer
import java.util.{Date, UUID}

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

  private implicit val userDbPickler: GetResult[User] = GetResult(r => User(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, LoginInfo(r.<<, r.<<)))
  private val AllUserColumns = "user_id, created_time, name_, email, avatar_url, activated, provider_id, provider_key"

  def retrieve(id: String) : Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where user_id = $id".as[User].headOption)

  def retrieve(loginInfo: LoginInfo): Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where provider_id = ${loginInfo.providerID} and provider_key = ${loginInfo.providerKey}".as[User].headOption)

  def create(u: User, authInfo: AuthInfo, documentTitle: String): Future[User] = {
    assert(u.userId == "")
    val time = System.currentTimeMillis()
    val userId = UUID.randomUUID().toString
    val createUser =
      sqlu"insert into users values ($userId, $time, ${u.name}, ${u.email}, ${u.avatarUrl.orNull: String}, ${u.activated}, ${u.loginInfo.providerID}, ${u.loginInfo.providerKey}, $authInfo)"
    val node = model.data.Node.create(documentTitle)
    val documentQuery = createDocumentQuery(userId, node, time)
    db.run(DBIO.seq(createUser +: documentQuery : _*).transactionally).map(_ => u.copy(userId = userId))
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
