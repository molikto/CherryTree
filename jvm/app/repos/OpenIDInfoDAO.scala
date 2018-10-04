package repos

import java.nio.ByteBuffer

import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.impl.providers.{OAuth1Info, OpenIDInfo}
import com.mohiva.play.silhouette.persistence.daos.DelegableAuthInfoDAO
import javax.inject.Inject
import play.api.db.slick.DatabaseConfigProvider

import scala.concurrent.{ExecutionContext, Future}

/**
  * The DAO to store the OpenID information.
  */
class OpenIDInfoDAO @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends DelegableAuthInfoDAO[OpenIDInfo] with DAOSlick {

  import profile.api._

  private def openIDInfoQuery(loginInfo: LoginInfo) = for {
    dbLoginInfo <- loginInfoQuery(loginInfo)
    dbOpenIDInfo <- OpenIDInfos if dbOpenIDInfo.loginInfoId === dbLoginInfo.id
  } yield dbOpenIDInfo

  import boopickle.Default._


  private def addAction(loginInfo: LoginInfo, authInfo: OpenIDInfo) =
    loginInfoQuery(loginInfo).result.head.flatMap { dbLoginInfo =>
      OpenIDInfos += OpenIDInfoRow(authInfo.id, dbLoginInfo.id.get, Pickle.intoBytes(authInfo.attributes).array())
    }.transactionally

  private def updateAction(loginInfo: LoginInfo, authInfo: OpenIDInfo) =
    openIDInfoQuery(loginInfo).
      map(dbOpenIDInfo => (dbOpenIDInfo.id, dbOpenIDInfo.attributes)).
      update((authInfo.id,  Pickle.intoBytes(authInfo.attributes).array())).transactionally

  /**
    * Finds the auth info which is linked with the specified login info.
    *
    * @param loginInfo The linked login info.
    * @return The retrieved auth info or None if no auth info could be retrieved for the given login info.
    */
  def find(loginInfo: LoginInfo): Future[Option[OpenIDInfo]] = {
    db.run(openIDInfoQuery(loginInfo).result.headOption).map { openIDInfos =>
      openIDInfos.map(dbOAuth1Info => OpenIDInfo(dbOAuth1Info.id, Unpickle[Map[String, String]].fromBytes(ByteBuffer.wrap(dbOAuth1Info.attributes))))
    }
  }

  /**
    * Adds new auth info for the given login info.
    *
    * @param loginInfo The login info for which the auth info should be added.
    * @param authInfo The auth info to add.
    * @return The added auth info.
    */
  def add(loginInfo: LoginInfo, authInfo: OpenIDInfo): Future[OpenIDInfo] =
    db.run(addAction(loginInfo, authInfo)).map(_ => authInfo)

  /**
    * Updates the auth info for the given login info.
    *
    * @param loginInfo The login info for which the auth info should be updated.
    * @param authInfo The auth info to update.
    * @return The updated auth info.
    */
  def update(loginInfo: LoginInfo, authInfo: OpenIDInfo): Future[OpenIDInfo] =
    db.run(updateAction(loginInfo, authInfo)).map(_ => authInfo)

  /**
    * Saves the auth info for the given login info.
    *
    * This method either adds the auth info if it doesn't exists or it updates the auth info
    * if it already exists.
    *
    * @param loginInfo The login info for which the auth info should be saved.
    * @param authInfo The auth info to save.
    * @return The saved auth info.
    */
  def save(loginInfo: LoginInfo, authInfo: OpenIDInfo): Future[OpenIDInfo] = {
    val query = loginInfoQuery(loginInfo).joinLeft(OpenIDInfos).on(_.id === _.loginInfoId)
    val action = query.result.head.flatMap {
      case (_, Some(_)) => updateAction(loginInfo, authInfo)
      case (_, None)               => addAction(loginInfo, authInfo)
    }
    db.run(action).map(_ => authInfo)
  }

  /**
    * Removes the auth info for the given login info.
    *
    * @param loginInfo The login info for which the auth info should be removed.
    * @return A future to wait for the process to be completed.
    */
  def remove(loginInfo: LoginInfo): Future[Unit] = {
    // val attributeQuery = for {
    //  dbOpenIDInfo <- openIDInfoQuery(loginInfo)
    //  dbOpenIDAttributes <- slickOpenIDAttributes.filter(_.id === dbOpenIDInfo.id)
    //} yield dbOpenIDAttributes
    // Use subquery workaround instead of join because slick only supports selecting
    // from a single table for update/delete queries (https://github.com/slick/slick/issues/684).
    val openIDInfoSubQuery = OpenIDInfos.filter(_.loginInfoId in loginInfoQuery(loginInfo).map(_.id))
    db.run(openIDInfoSubQuery.delete).map(_ => ())
  }
}
