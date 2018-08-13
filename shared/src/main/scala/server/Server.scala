package server

import java.nio.ByteBuffer

import model._
import api._
import model.data.{Node, Text, Unicode}
import model.ot.Rebased

import scala.collection.mutable
import scala.util.Random

trait Server extends Api {
  def debugSave(a: String, bs: Array[Byte])
  def debugLoad(a: String): Array[Byte]

  // states, now in single thread fashion

  case class ClientInfo(version: Int, lastSeen: Long)

  private var document = {
    val bs = debugLoad("saved")
    val res = if (bs.isEmpty) Node.create()
    else Unpickle[Node](Node.pickler).fromBytes(ByteBuffer.wrap(bs))
    model.oldDocVersion = false
    res
  }
  private var changes = Seq.empty[transaction.Node]
  def version: Int = changes.size
  private val clients: mutable.Map[Authentication.Token, ClientInfo] = mutable.Map.empty
  private var debugHistoryDocuments = Seq(document)

  def debugDocument = document
  def debugChanges = changes

  private def onlineCount = clients.values.count(_.lastSeen > System.currentTimeMillis() - ApiConstants.ClientDeathTime)

  override def init(token: Authentication.Token): Either[ApiError, ClientInit] = synchronized {
    val state = ClientInit(
      document,
      version,
      ServerStatus(onlineCount + 1)
    )
    clients.update(token, ClientInfo(version, System.currentTimeMillis()))
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
        val diff = version - cached.version
         if (diff < 0) {
           Left(ApiError.ClientVersionIsOlderThanServerCache)
        } else if (diff > 0) {
           Left(ApiError.ClientVersionIsHigherThanServerCache)
        } else {
          Right(changes.drop(version))
        }
    }
  }

  override def change(authentication: Authentication.Token, clientVersion: Int, ts: Seq[transaction.Node], mode: Option[model.mode.Node], debugClientDoc: data.Node): ErrorT[ClientUpdate] = synchronized {
    checkWriteStateConsistency(authentication, clientVersion).map { ws =>
      try {
        if (debug_model) {
          assert(debugClientDoc == debugHistoryDocuments(clientVersion))
        }
        val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseT(ws.flatten, ts)
        var debugTopDocument = document
        document = operation.Node.applyT(transformed, document)
        changes = changes ++ transformed
        if (transformed.nonEmpty) {
          debugSave("saved", Pickle.intoBytes(document)(implicitly, Node.pickler).array())
        }
        if (debug_model) {
          for (t <- transformed) {
            debugTopDocument = operation.Node.apply(t, debugTopDocument)
            debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
          }
          assert(operation.Node.apply(wws, operation.Node.applyT(ts, debugHistoryDocuments(clientVersion))) == document)
        }
        clients.update(authentication, ClientInfo(version, System.currentTimeMillis()))
        // LATER don't accept conflicting items
        ClientUpdate(ws, ts.size, version, ServerStatus(onlineCount))
      } catch {
        case e: Throwable => throw e
      }
    }
  }
}
