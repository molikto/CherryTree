package shared.server

import shared.api.{Api, ApiError, Authentication, ClientInit, ClientUpdate, ErrorT}
import shared.data0.Node
import shared.ot.Rebased

import scala.collection.mutable

class CherryTreeServer extends Api {

  // states, now in single thread fashion
  def emptyDocument = Node("", Seq.empty)
  private var document = emptyDocument
  private var changes = Seq.empty[Node.Transaction]
  def version: Int = changes.size
  private val clients: mutable.Map[Authentication.Token, Int] = mutable.Map.empty

  def debugDocument = document
  def debugChanges = changes

  override def init(token: Authentication.Token): Either[ApiError, ClientInit] = synchronized {
    val state = ClientInit(document, version)
    clients.update(token, version)
    Right(state)
  }

  /**
    * @return unread transactions
    */
//  private def checkReadStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[Node.Transaction]] = synchronized {
//    clients.get(authentication) match {
//      case None =>
//        Left(ApiError.InvalidToken)
//      case Some(cached) =>
//        Right(changes.drop(version))
//    }
//  }


  /**
    *
    * @param version current client version
    * @return
    */
  private def checkWriteStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[Node.Transaction]] = synchronized {
    clients.get(authentication) match {
      case None =>
        Left(ApiError.InvalidToken)
      case Some(cached) =>
        val diff = version - cached
         if (diff < 0) {
           Left(ApiError.ClientVersionIsOlderThanServerCache)
        } else if (diff > 0) {
           Left(ApiError.ClientVersionIsHigherThanServerCache)
        } else {
          Right(changes.drop(version))
        }
    }
  }

  override def change(authentication: Authentication.Token, clientVersion: Int, ts: Seq[Node.Transaction]): ErrorT[ClientUpdate] = synchronized {
    checkWriteStateConsistency(authentication, clientVersion).map { ws =>
      try {
        val Rebased(conflicts, (wws, transformed)) = Node.Ot.rebaseT(ws.flatten, ts)
        document = Node.Ot.applyT(transformed, document)
        changes = changes ++ transformed
        clients.update(authentication, version)
        // TODO don't accept conflicting items
        ClientUpdate(ws, ts.size, version)
      } catch {
        case e: Throwable => throw e
      }
    }
  }
}
