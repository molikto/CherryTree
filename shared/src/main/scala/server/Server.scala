package server

import java.nio.ByteBuffer
import java.util.UUID

import model._
import api._
import model.data.{Node, Text, Unicode}
import model.ot.Rebased

import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}

object Server {
  trait User {
    def userId: UUID
    def toCollabrator: Collaborator
  }
  case class InitResult(root: model.data.Node, version: Int, creators: Map[UUID, UUID])
}

abstract class Server[CTX <: Server.User](documentId: UUID, serverInit: Server.InitResult) {


  // states, now in single thread fashion
  private var document: Node = serverInit.root
  private var baseVersion: Int = serverInit.version
  private val creators: mutable.Map[UUID, UUID] = mutable.Map.empty ++ serverInit.creators

  private var changes = Seq.empty[(transaction.Node, UUID)]

  private var collaborators = Seq.empty[CTX]

  def version: Int = baseVersion + changes.size

  def debugDocument = document
  def debugChanges = changes


  def init(ctx: CTX, permissionLevel: Int): InitResponse = {
    val state = InitResponse(
      document,
      version,
      permissionLevel,
      serverStatus(ctx),
    )
    if (debug_transmit) {
      println(state)
    }
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


  def change(ctx: CTX, changeRequest: ChangeRequest, permissionLevel: Int): Try[ChangeResponse] = {
    val ChangeRequest(clientVersion, ts, mode, debugClientDoc) = changeRequest
    if (permissionLevel >= PermissionLevel.Edit) {

    } else if (permissionLevel >= PermissionLevel.Comment) {
//      ts.exists(t => {
//        t._1.exists(_.)
//      })
    } else if (permissionLevel >= PermissionLevel.Read) {
      if (ts.nonEmpty) {
        return Failure(ApiError.PermissionViolation)
      }
    } else {
      return Failure(ApiError.PermissionViolation)
    }
    def normalCase() = {
      val after = changes.drop(clientVersion - baseVersion)
      //          if (debug_transmit && debugClientDoc != 0) {
      //            val oldCode = debugHistoryDocuments(clientVersion).hashCode()
      //            if (debugClientDoc != oldCode) {
      //              throw new IllegalStateException(s"transmit error?? $clientVersion, ${document.size} $debugClientDoc $oldCode")
      //            }
      //          }
      def normalNormalCase() = {
        val ws = after.map(_._1)
        val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseTIded(ws.flatten, ts)
        //var debugTopDocument = document

        val (transformedDocument, diffs) = transformed.foldLeft((document, Seq.empty[Seq[model.operation.Node.Diff]])) {
          (model, c) => operation.Node.applyWithDiff(c._1, model._1) match { case (d, nn) => (nn, model._2 :+ d) }
        }
        assert(diffs.size == transformed.size)
        val toPersist = transformed.zip(diffs).map(a => (a._1._1, a._1._2, a._2))
        //
        persist(ctx, toPersist)
        // commit to memory
        document = transformedDocument
        changes = changes ++ transformed
        for (tt <- transformed) {
          for (t <- tt._1) {
            t match {
              case model.operation.Node.Insert(_, cs) =>
                cs.foreach(c => {
                  creators += (c.uuid -> ctx.userId)
                })
              case _ =>
            }
          }
        }
        val cu = ChangeResponse(ws, ts.size, version, serverStatus(ctx))
        //          if (transformed.nonEmpty) {
        //            debugSave("saved", Pickle.intoBytes(document)(implicitly, Node.pickler).array())
        //          }
        //          if (debug_transmit) {
        //            for (t <- transformed) {
        //              debugTopDocument = operation.Node.apply(t, debugTopDocument)
        //              debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
        //            }
        //          }
        // LATER don't accept conflicting items
        Success(cu)
      }
      ts match {
        case head +: tail =>
          val index = after.indexWhere(_._2 == head._2)
          if (index < 0) {
            // this is not a stale request
            normalNormalCase()
          } else {
            val take = after.drop(index).zipWithIndex.takeWhile(a => a._1._2 == ts(a._2)._2).size
            val winners = after.take(index).map(_._1)
            if (model.debug_model) println(s"a request that ready been taken care of, with ${winners.size} and $take")
            Success(ChangeResponse(winners, take, clientVersion + winners.size + take, serverStatus(ctx)))
          }
        case _ =>
          normalNormalCase()
      }
    }
    if (clientVersion < baseVersion) {
      loadChanges(clientVersion, baseVersion) match {
        case Some(list) =>
          assert(list.size == baseVersion - clientVersion)
          changes = list ++ changes
          baseVersion = clientVersion
          normalCase()
        case None =>
          Failure(ApiError.ClientVersionTooOld)
      }
    } else if (clientVersion > version) {
      Failure(ApiError.ClientVersionIsHigherThanServerCache)
    } else {
      normalCase()
    }
  }

  def persist(ctx: CTX, changes: Seq[(model.transaction.Node, UUID, Seq[model.operation.Node.Diff])]): Unit = {
  }

  def loadChanges(from: Int, until: Int): Option[Seq[(model.transaction.Node, UUID)]] = None

  def serverStatus(ctx: CTX): ServerStatus = {
    ServerStatus(
      ctx.toCollabrator,
      collaborators.filter(_.userId != ctx.userId).map(u => u.toCollabrator))
  }

  def addCollabrator(user: CTX) = {
    if (!collaborators.exists(_.userId == user.userId)) collaborators = collaborators :+ user
  }

  def removeCollabrator(user: UUID) = {
    collaborators = collaborators.filter(_.userId != user)
  }
}
