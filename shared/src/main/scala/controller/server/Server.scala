package controller.server

import model._
import controller.api._
import model.ot.Rebased

import scala.collection.mutable

class Server extends Api {

  // states, now in single thread fashion
  private var document = data.Node.empty
  private var changes = Seq.empty[transaction.Node]
  def version: Int = changes.size
  private val clients: mutable.Map[Authentication.Token, Int] = mutable.Map.empty
  private var debugHistoryDocuments = Seq(document)

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
//  private def checkReadStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[transaction.Node]] = synchronized {
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
  private def checkWriteStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[transaction.Node]] = synchronized {
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

  override def change(authentication: Authentication.Token, clientVersion: Int, ts: Seq[transaction.Node], debugClientDoc: data.Node): ErrorT[ClientUpdate] = synchronized {
    checkWriteStateConsistency(authentication, clientVersion).map { ws =>
      try {
        if (debugModel) {
          assert(debugClientDoc == debugHistoryDocuments(clientVersion))
        }
        val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseT(ws.flatten, ts)
        var debugTopDocument = document
        document = operation.Node.applyT(transformed, document)
        changes = changes ++ transformed
        for (t <- transformed) {
          debugTopDocument = operation.Node.apply(t, debugTopDocument)
          debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
        }
        if (debugModel) {
          assert(operation.Node.apply(wws, operation.Node.applyT(ts, debugHistoryDocuments(clientVersion))) == document)
        }
        clients.update(authentication, version)
        // LATER don't accept conflicting items
        ClientUpdate(ws, ts.size, version)
      } catch {
        case e: Throwable => throw e
      }
    }
  }
}
