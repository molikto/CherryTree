package repos


import javax.inject.Inject
import models.AuthToken
import repos.AuthTokenDAOImpl._
import org.joda.time.DateTime
import play.api.db.slick.DatabaseConfigProvider

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/**
  */
class AuthTokenDAO @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends DAOSlick {

  /**
    * Finds a token by its ID.
    *
    * @param id The unique token ID.
    * @return The found token or None if no token for the given ID could be found.
    */
  def find(id: String): Future[Option[AuthToken]] = Future.successful(tokens.get(id))

  /**
    * Finds expired tokens.
    *
    * @param dateTime The current date time.
    */
  def findExpired(dateTime: DateTime): Future[Seq[AuthToken]] = Future.successful {
    tokens.filter {
      case (_, token) =>
        token.expiry.isBefore(dateTime)
    }.values.toSeq
  }

  /**
    * Saves a token.
    *
    * @param token The token to save.
    * @return The saved token.
    */
  def save(token: AuthToken): Future[AuthToken] = {
    tokens += (token.id -> token)
    Future.successful(token)
  }

  /**
    * Removes the token for the given ID.
    *
    * @param id The ID for which the token should be removed.
    * @return A future to wait for the process to be completed.
    */
  def remove(id: String): Future[Unit] = {
    tokens -= id
    Future.successful(())
  }
}

/**
  * The companion object.
  */
object AuthTokenDAOImpl {

  /**
    * The list of tokens.
    */
  val tokens: mutable.HashMap[String, AuthToken] = mutable.HashMap()
}