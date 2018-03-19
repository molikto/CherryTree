package shared.server

import shared.api.{Api, ApiError, Authentication, ClientInit, ClientUpdate, ErrorT}
import shared.data0.Node

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
        val transformed = Transaction.rebase(ws, ts, RebaseConflict.all)._2
        val versionMore = transformed.size
        document = Document(document.version + versionMore, Transaction.apply(document.root, transformed))
        changes = changes ++ transformed
        ClientStateUpdate(DocumentUpdate(ws, versionMore))
      } catch {
        case e: Throwable => throw e
      }
    }
  }
}
