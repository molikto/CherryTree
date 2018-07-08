package controller.server

import model._
import controller.api._
import model.ot.Rebased

import scala.collection.mutable

class CherryTreeServer extends Api {

  // states, now in single thread fashion
  def emptyDocument = data.Node(data.Content.Paragraph(data.Paragraph.empty), Seq.empty)
  private var document = emptyDocument
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

  override def change(authentication: Authentication.Token, clientVersion: Int, ts: Seq[transaction.Node]): ErrorT[ClientUpdate] = synchronized {
    checkWriteStateConsistency(authentication, clientVersion).map { ws =>
      try {
        val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseT(ws.flatten, ts)
        document = operation.Node.applyT(transformed, document)
        changes = changes ++ transformed
        var debugTopDocument = document
        for (t <- transformed) {
          debugTopDocument = operation.Node.apply(t, debugTopDocument)
          debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
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
