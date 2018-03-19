package shared.server

import shared.api.{Api, ApiError, Authentication, ClientInit, ClientUpdate, ErrorT}
import shared.data0.Node
import shared.ot.Rebased

import scala.collection.mutable

class CherryTreeServer extends Api {

  // states, now in single thread fashion
  private val emptyDocument = Node("", Seq.empty)
  private var document = emptyDocument
  private var version: Int = 0
  private var changes = Seq.empty[Node.Transaction]
  private val clients: mutable.Map[Authentication.Token, Int] = mutable.Map.empty

  def debugDocument = document
  def debugChanges = changes

  override def init(token: Authentication.Token): ClientInit = synchronized {
    val state = ClientInit(document, version)
    clients.update(token, version)
    state
  }

  private def checkReadStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[Node.Transaction]] = synchronized {
    clients.get(authentication) match {
      case None =>
        Left(ApiError.InvalidToken)
      case Some(cached) =>
        Right(changes.drop(version))
    }
  }

  private def checkWriteStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[Node.Transaction]] = synchronized {
    clients.get(authentication) match {
      case None =>
        Left(ApiError.InvalidToken)
      case Some(cached) =>
        val diff = version - cached
         if (diff < 0) {
          Left(ApiError.ClientVersionIsOlderThanServerCache)
        } else {
          Right(changes.drop(version))
        }
    }
  }

  override def change(authentication: Authentication.Token, version: Int, ts: Seq[Node.Transaction]): ErrorT[ClientUpdate] = synchronized {
    checkWriteStateConsistency(authentication, version).map { ws =>
      try {
        val Rebased(conflicts, (wws, transformed)) = Node.Ot.rebaseT(ws.flatten, ts)
        val versionMore = transformed.size
        document = Node.Ot.applyT(transformed, document)
        changes = changes ++ transformed
        // TODO don't accept conflicting items
        ClientUpdate(ws, ts.size)
      } catch {
        case e: Throwable => throw e
      }
    }
  }
}
