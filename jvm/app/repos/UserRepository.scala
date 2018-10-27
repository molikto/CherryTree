package repos

import java.util.UUID

import javax.inject.Inject
import com.mohiva.play.silhouette.api.{AuthInfo, LoginInfo}
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.impl.providers.CommonSocialProfile
import models.User

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

class UserRepository @Inject()(implicit ex: ExecutionContext) extends IdentityService[User] with AuthInfoRepository {

  def retrieve(id: String) : Future[Option[User]] = ???

  def retrieve(loginInfo: LoginInfo): Future[Option[User]] = ???

  def save(user: User, authInfo: AuthInfo): Future[User] = ???

  def save(profile: CommonSocialProfile, authInfo: AuthInfo): Future[User] = ???

  def update(user: User): Future[User] = ???

  private def profileName(profile: CommonSocialProfile) =
    profile.firstName -> profile.lastName match {
      case (Some(f), Some(l)) => Some(f + " " + l)
      case (Some(f), None) => Some(f)
      case (None, Some(l)) => Some(l)
      case _ => None
    }


  /**
    * this is only used by password authenticator to find a password auth info
    */
  override def find[T <: AuthInfo](loginInfo: LoginInfo)(implicit tag: ClassTag[T]): Future[Option[T]] = ???


  /**
    * this is used when user change/reset password and also hasher needs to be updated
    */
  override def update[T <: AuthInfo](loginInfo: LoginInfo, authInfo: T): Future[T] = ???

  override def add[T <: AuthInfo](loginInfo: LoginInfo, authInfo: T): Future[T] = throw new IllegalStateException("This is not used")

  override def save[T <: AuthInfo](loginInfo: LoginInfo, authInfo: T): Future[T] = throw new IllegalStateException("This is not used")

  override def remove[T <: AuthInfo](loginInfo: LoginInfo)(implicit tag: ClassTag[T]): Future[Unit] = throw new IllegalStateException("This is not used")
}
