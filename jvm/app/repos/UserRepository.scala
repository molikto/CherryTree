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
import model._
import api._
import play.api.Environment
import play.api.libs.json.{JsSuccess, Json}


@Singleton
class UserRepository @Inject() (
  env: Environment, protected val dbConfigProvider: DatabaseConfigProvider)
  (implicit ex: ExecutionContext) extends IdentityService[User] with AuthInfoRepository with DatabaseAccessing {


  def newUserDocument(): model.data.Node = model.data.Node.create("New document")

//  private lazy val initDocument = {
//    val temp = env.classLoader.getResourceAsStream("docinit.json")
//    import model._
//    Json.fromJson[model.data.Node](Json.parse(temp)) match {
//      case JsSuccess(node, path) =>
//        node
//      case _ =>
//        model.data.Node.create("New document")
//    }
//  }

  import utils.MyPostgresProfile.plainApi._

  private implicit val userDbPickler: GetResult[User] = GetResult(r => User(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, LoginInfo(r.<<, r.<<)))
  private val AllUserColumns = "user_id, created_time, name_, email, avatar_url, activated, provider_id, provider_key"

  def retrieve(email: String): Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where email = $email".as[User].headOption)

  def retrieve(id: UUID) : Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where user_id = $id".as[User].headOption)

  def retrieve(loginInfo: LoginInfo): Future[Option[User]] =
    db.run(sql"select #$AllUserColumns from users where provider_id = ${loginInfo.providerID} and provider_key = ${loginInfo.providerKey}".as[User].headOption)

  def create(u: User, authInfo: AuthInfo, initDocument: model.data.Node): Future[User] = {
    val time = System.currentTimeMillis()
    val createUser =
      sqlu"insert into users values (${u.userId}, $time, ${u.name}, ${u.email}, ${u.avatarUrl.orNull: String}, ${u.activated}, ${u.loginInfo.providerID}, ${u.loginInfo.providerKey}, $authInfo)"
    val (docId, documentQuery) = createDocumentQuery(u.userId, initDocument, time)
    val additionalDocumentQuery =
      sqlu"insert into permissions values (${u.userId}, $docId, ${PermissionLevel.Read})"
    db.run(DBIO.seq(createUser +: documentQuery :+ additionalDocumentQuery : _*).transactionally).map(_ => u)
  }

  def permission(userId: UUID, documentId: UUID): _root_.scala.concurrent.Future[Int] = {
    db.run(sql"select permission_level from permissions where document_id = $documentId and user_id = $userId".as[Int].headOption).map(_.getOrElse(0))
  }

  def deletePermission(documentId: UUID, userId: UUID): Future[Unit] = {
    db.run(sql"delete from permissions where user_id = $userId and document_id = $documentId".as[Int]).map(_ => Unit)
  }

  def addPermission(documentId: UUID, userId: UUID, level: Int): Future[Unit] = {
    db.run(sql"insert into permissions values ($userId, $documentId, $level) on conflict on constraint permissions_pkey do update set permission_level = EXCLUDED.permission_level".as[Int]).map(_ => Unit)
  }

  def hasPermission(userId: UUID, documentId: UUID, level: Int): _root_.scala.concurrent.Future[Boolean] = {
    db.run(sql"select document_id from permissions where document_id = $documentId and user_id = $userId and permission_level >= $level".as[UUID].headOption).map(_.isDefined)
  }

  def collabrators(uid: UUID, did: UUID): Future[Seq[(Collaborator, Int)]] = {
    db.run(sql""" select users.email, users.name_, users.avatar_url, permissions.permission_level from users, permissions
       where users.user_id = permissions.user_id and permissions.document_id = ${did}""".as[(String, String, Option[String], Int)]).map(_.map(d => (Collaborator(d._1, d._2, d._3), d._4)))
  }

  def activate(userId: UUID, activate: Boolean = true): Future[Option[Unit]] =
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
