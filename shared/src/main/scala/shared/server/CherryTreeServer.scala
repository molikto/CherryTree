package shared.server

import shared.api.{Api, ApiError, Authentication}
import shared.data._

import scala.collection.mutable

class CherryTreeServer extends Api {

  // states, now in single thread fashion
  private val emptyDocument = Document.empty(Node.newId())
  private var document = emptyDocument
  private var changes = Seq.empty[Transaction]
  private val clients: mutable.Map[Authentication.Token, ClientStateSnapshot] = mutable.Map.empty

  def debugDocument = document
  def debugChanges = changes

  override def init(token: Authentication.Token): ClientState = synchronized {
    val state = ClientState(token, document, document.initialMode)
    clients.update(token, ClientStateSnapshot(state))
    state
  }

  private def checkReadStateConsistency(snapshot: ClientStateSnapshot): ErrorT[Seq[Transaction]] = synchronized {
    clients.get(snapshot.authentication) match {
      case None =>
        Left(ApiError.InvalidToken)
      case Some(cached) =>
        Right(changes.drop(snapshot.document.version))
    }
  }

  private def checkWriteStateConsistency(snapshot: ClientStateSnapshot): ErrorT[Seq[Transaction]] = synchronized {
    clients.get(snapshot.authentication) match {
      case None =>
        Left(ApiError.InvalidToken)
      case Some(cached) =>
        val diff = snapshot.document.version - cached.document.version
         if (diff < 0) {
          Left(ApiError.ClientVersionIsOlderThanServerCache)
        } else {
          Right(changes.drop(snapshot.document.version))
        }
    }
  }

  override def change(snapshot: ClientStateSnapshot, ts: Seq[Transaction]): ErrorT[ClientStateUpdate] = synchronized {
    checkWriteStateConsistency(snapshot).map { ws =>
      snapshot.document.debugRoot.foreach(root => {
        if (root != Transaction.apply(emptyDocument.root, changes.take(snapshot.document.version))) {
          new IllegalStateException("Client server state diverged")
        }
      })
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

  override def diffSince(snapshot: ClientStateSnapshot): ErrorT[ClientStateUpdate] = synchronized {
    checkReadStateConsistency(snapshot).map { ws =>
      ClientStateUpdate(DocumentUpdate(ws, 0))
    }
  }

  override def test(): ApiError = ApiError.InvalidToken
}
