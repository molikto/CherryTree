package server

import java.nio.ByteBuffer
import java.util.UUID

import model._
import api._
import model.data.{Node, Text, Unicode}
import model.ot.Rebased

import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}

class Server(documentId: String) {

  def debugSave(a: String, bs: Array[Byte]) = {}
  def debugLoad(a: String): Array[Byte] = new Array[Byte](0)

  // states, now in single thread fashion

  case class ClientInfo(
    version: Int,
    lastSeen: Long,
    lastAccepted: Int,
    lastWs: Seq[transaction.Node])

  private var document = {
    val bs = debugLoad("saved")
    val res = if (bs.isEmpty) Node.create()
    else Unpickle[Node](Node.pickler).fromBytes(ByteBuffer.wrap(bs))(unpickleState)
    res
  }
  private var changes = Seq.empty[transaction.Node]
  def version: Int = changes.size
  private val clients: mutable.Map[String, ClientInfo] = mutable.Map.empty
  private var debugHistoryDocuments = Seq(document)

  def debugDocument = document
  def debugChanges = changes

  private def onlineCount = clients.values.count(_.lastSeen > System.currentTimeMillis() - ApiConstants.ClientDeathTime)

  def init(): InitResponse = synchronized {
    val session = UUID.randomUUID().toString
    val state = InitResponse(
      session,
      document,
      version,
      ServerStatus(onlineCount + 1, false, false)
    )
    if (debug_transmit) {
      println(state)
    }
    clients.update(session, ClientInfo(version, System.currentTimeMillis(), 0, Seq.empty))
    state
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

  def serverStatus: ServerStatus = ServerStatus(onlineCount, false, false)

  def change(changeRequest: ChangeRequest): Try[ChangeResponse] = synchronized {
    val ChangeRequest(session, clientVersion, ts, mode, debugClientDoc) = changeRequest
    clients.get(session) match {
      case None =>
        Failure(ApiError.InvalidToken)
      case Some(cached) =>
        val diff = clientVersion - cached.version
        if (diff < 0) {
          if (ts.size >= cached.lastAccepted) {
            if (model.debug_model) println("previous data back failed")
            Success(ChangeResponse(cached.lastWs, cached.lastAccepted, cached.version, serverStatus))
          } else {
            Failure(ApiError.ClientVersionIsOlderThanServerCache)
          }
        } else if (diff > 0) {
          Failure(ApiError.ClientVersionIsHigherThanServerCache)
        } else {
          val ws = changes.drop(clientVersion)
          if (debug_transmit && debugClientDoc != 0) {
            val oldCode = debugHistoryDocuments(clientVersion).hashCode()
            if (debugClientDoc != oldCode) {
              throw new IllegalStateException(s"transmit error?? $clientVersion, ${document.size} $debugClientDoc $oldCode")
            }
          }
          val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseT(ws.flatten, ts)
          var debugTopDocument = document
          document = operation.Node.applyT(transformed, document)
          changes = changes ++ transformed
          if (transformed.nonEmpty) {
            debugSave("saved", Pickle.intoBytes(document)(pickleState, Node.pickler).array())
          }
          if (debug_transmit) {
            for (t <- transformed) {
              debugTopDocument = operation.Node.apply(t, debugTopDocument)
              debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
            }
          }
          val cu = ChangeResponse(ws, ts.size, version, serverStatus)
          clients.update(session, ClientInfo(cu.finalVersion, System.currentTimeMillis(), cu.acceptedLosersCount, cu.winners))
          // LATER don't accept conflicting items
          Success(cu)
        }
    }
  }
}
