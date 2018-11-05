package utils.auth

import java.util.UUID

import api.PermissionLevel
import com.mohiva.play.silhouette.api.{Authenticator, Authorization}
import models.User
import play.api.mvc.Request
import repos.UserRepository

import scala.concurrent.Future



case class HasPermission[A <: Authenticator](documentId: UUID, users: UserRepository, level: Int = PermissionLevel.ReadOnly) extends Authorization[User, A] {
  override def isAuthorized[B](identity: User, authenticator: A)(implicit request: Request[B]): Future[Boolean] = users.hasPermission(identity.userId, documentId, level)
}
