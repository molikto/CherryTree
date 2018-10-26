package repos



import com.mohiva.play.silhouette.api.LoginInfo
import models.User
import javax.inject.Inject
import play.api.db.slick.DatabaseConfigProvider

import scala.concurrent.{ExecutionContext, Future}

/**
  * Give access to the user object using Slick
  */
class UserDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) extends DAOSlick {

  import profile.api._

  /**
    * Finds a user by its login info.
    *
    * @param loginInfo The login info of the user to find.
    * @return The found user or None if no user for the given login info could be found.
    */
  def find(loginInfo: LoginInfo): Future[Option[User]] = {
    val userQuery = for {
      dbLoginInfo <- loginInfoQuery(loginInfo)
      dbUserLoginInfo <- UserLoginInfos.filter(_.loginInfoId === dbLoginInfo.id)
      dbUser <- Users.filter(_.id === dbUserLoginInfo.userId)
    } yield dbUser
    db.run(userQuery.result.headOption).map { dbUserOption =>
      dbUserOption.map { user =>
        User(user.userId, loginInfo, user.name, user.email, user.avatarUrl, user.activated)
      }
    }
  }

  /**
    * Finds a user by its user ID.
    *
    * @param userId The ID of the user to find.
    * @return The found user or None if no user for the given ID could be found.
    */
  def find(userId: String): Future[Option[User]] = {
    val query = for {
      dbUser <- Users.filter(_.id === userId.toString)
      dbUserLoginInfo <- UserLoginInfos.filter(_.userId === dbUser.id)
      dbLoginInfo <- LoginInfos.filter(_.id === dbUserLoginInfo.loginInfoId)
    } yield (dbUser, dbLoginInfo)
    db.run(query.result.headOption).map { resultOption =>
      resultOption.map {
        case (user, loginInfo) =>
          User(
            user.userId,
            LoginInfo(loginInfo.providerId, loginInfo.providerKey),
            user.name,
            user.email,
            user.avatarUrl,
            user.activated)
      }
    }
  }

  /**
    * Saves a user.
    *
    * @param user The user to save.
    * @return The saved user.
    */
  def save(user: User): Future[User] = {
    val dbUser = UserRow(user.userId.toString, user.name, user.email, user.avatarUrl, user.activated)
    val dbLoginInfo = LoginInfoRow(None, user.loginInfo.providerID, user.loginInfo.providerKey)
    // We don't have the LoginInfo id so we try to get it first.
    // If there is no LoginInfo yet for this user we retrieve the id on insertion.
    val loginInfoAction = {
      val retrieveLoginInfo = LoginInfos.filter(
        info => info.providerId === user.loginInfo.providerID &&
          info.providerKey === user.loginInfo.providerKey).result.headOption
      val insertLoginInfo = LoginInfos.returning(LoginInfos.map(_.id)).
        into((info, id) => info.copy(id = Some(id))) += dbLoginInfo
      for {
        loginInfoOption <- retrieveLoginInfo
        loginInfo <- loginInfoOption.map(DBIO.successful).getOrElse(insertLoginInfo)
      } yield loginInfo
    }
    // combine database actions to be run sequentially
    val actions = (for {
      _ <- Users.insertOrUpdate(dbUser)
      loginInfo <- loginInfoAction
      _ <- UserLoginInfos += UserLoginInfoRow(dbUser.userId, loginInfo.id.get)
    } yield ()).transactionally
    // run actions and return user afterwards
    db.run(actions).map(_ => user)
  }
}
