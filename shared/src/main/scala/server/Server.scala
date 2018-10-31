package server

import java.nio.ByteBuffer
import java.util.UUID

import model._
import api._
import model.data.{Node, Text, Unicode}
import model.ot.Rebased

import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}

class Server(documentId: String, private var document: Node, private var baseVersion: Int) {

  // states, now in single thread fashion

  case class ClientInfo(
    version: Int,
    lastSeen: Long,
    lastAccepted: Int,
    lastWs: Seq[transaction.Node])

  private var changes = Seq.empty[transaction.Node]

  def version: Int = baseVersion + changes.size
  private val clients: mutable.Map[String, ClientInfo] = mutable.Map.empty
//  private var debugHistoryDocuments = Seq(document)

  def debugDocument = document
  def debugChanges = changes

  private def onlineCount = clients.values.count(_.lastSeen > System.currentTimeMillis() - ApiConstants.ClientDeathTime)

  def init(): InitResponse = {
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

  def change(changeRequest: ChangeRequest): Try[ChangeResponse] = {
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

          def normalCase() = {

            val ws = changes.drop(clientVersion - baseVersion)
            //          if (debug_transmit && debugClientDoc != 0) {
            //            val oldCode = debugHistoryDocuments(clientVersion).hashCode()
            //            if (debugClientDoc != oldCode) {
            //              throw new IllegalStateException(s"transmit error?? $clientVersion, ${document.size} $debugClientDoc $oldCode")
            //            }
            //          }
            val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseT(ws.flatten, ts)
            //var debugTopDocument = document

            val (transformedDocument, diffs) = transformed.foldLeft((document, Seq.empty[Seq[model.operation.Node.Diff]])) {
              (model, c) => operation.Node.applyWithDiff(c, model._1) match { case (d, nn) => (nn, model._2 :+ d) }
            }
            assert(diffs.size == transformed.size)
            val toPersist = transformed.zip(diffs)
            //
            persist(toPersist)
            // commit to memory
            document = transformedDocument
            changes = changes ++ transformed
            val cu = ChangeResponse(ws, ts.size, version, serverStatus)
            //          if (transformed.nonEmpty) {
            //            debugSave("saved", Pickle.intoBytes(document)(implicitly, Node.pickler).array())
            //          }
            //          if (debug_transmit) {
            //            for (t <- transformed) {
            //              debugTopDocument = operation.Node.apply(t, debugTopDocument)
            //              debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
            //            }
            //          }
            clients.update(session, ClientInfo(cu.finalVersion, System.currentTimeMillis(), cu.acceptedLosersCount, cu.winners))
            // LATER don't accept conflicting items
            Success(cu)
          }
          if (clientVersion < baseVersion) {
            loadChanges(clientVersion, baseVersion) match {
              case Some(list) =>
                assert(list.size == baseVersion - clientVersion)
                changes = list ++ changes
                baseVersion = clientVersion
                normalCase()
              case None =>
                Failure(ApiError.ClientVersionIsHigherThanServerCache)
            }
          } else {
            normalCase()
          }
        }
    }
  }

  def persist(changes: Seq[(model.transaction.Node, Seq[model.operation.Node.Diff])]): Unit = {
  }

  def loadChanges(from: Int, until: Int): Option[Seq[model.transaction.Node]] = None
}
