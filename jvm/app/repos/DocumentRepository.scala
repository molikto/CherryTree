package repos

import java.nio.ByteBuffer
import java.util.{Date, UUID}

import api.PermissionLevel
import javax.inject.{Inject, Singleton}
import com.mohiva.play.silhouette.api.{AuthInfo, LoginInfo}
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.impl.providers.{CommonSocialProfile, OAuth1Info, OAuth2Info, OpenIDInfo}
import models.User
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import play.api.libs.json._
import model._
import api._
import model.data.{Content, Node}
import model.operation.Node
import model.transaction.Node
import server.Server

import scala.util.Success

object DocumentRepository {
  case class NodeResult(nodeId: UUID, childs: Seq[UUID], attributes: JsValue, content: model.data.Content, creatorId: UUID)
}

@Singleton
class DocumentRepository@Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
  (implicit ex: ExecutionContext) extends  DatabaseAccessing {



  import utils.MyPostgresProfile.plainApi._

  def create(userId: UUID, node: model.data.Node) = {
    db.run(DBIO.seq(createDocumentQuery(userId, node, System.currentTimeMillis()) : _*).transactionally)
  }


  def nodeInfo(did: UUID, nid: UUID): Future[Option[NodeInfo]] = {
    db.run(
      sql"""
            select nodes.created_time, nodes.last_updated_time, users.email, users.name_, users.avatar_url
             from nodes, users where nodes.document_id = $did and nodes.node_id = $nid and users.user_id = nodes.creator_id
         """.as[NodeInfo].headOption)
  }


  def delete(uuid: UUID): Future[Unit] = {
    db.run(DBIO.seq(
      sqlu"delete from changes where document_id = $uuid",
      sqlu"delete from nodes where document_id = $uuid",
      sqlu"delete from permissions where document_id = $uuid",
      sqlu"delete from documents where document_id = $uuid"
    ).transactionally)
  }

  private val ListColumns = "documents.document_id, nodes.cont, documents.created_time, documents.last_updated_time, permissions.permission_level"

  def head(uid: UUID, did: UUID): Future[Option[ListResult]] = {
    db.run(
      sql"""select #$ListColumns
           from documents, nodes, permissions where
              permissions.user_id = $uid and
              permissions.document_id = $did and
              permissions.permission_level > ${0} and
              documents.document_id = $did and
              documents.root_node_id = nodes.node_id
        """.as[ListResult].headOption)

  }



  def list(uid: UUID): Future[Seq[ListResult]] = {
    db.run(
      sql"""select #$ListColumns
           from documents, nodes, permissions where
              permissions.user_id = $uid and
              permissions.document_id = documents.document_id and
              permissions.permission_level > ${0} and
              documents.root_node_id = nodes.node_id
           order by documents.last_updated_time desc
        """.as[ListResult])
  }

  def init(a: UUID): Future[Server.InitResult] = {
    val query = sql"select current_version, root_node_id from documents where document_id = $a".as[(Int, UUID)].head.flatMap {
      case (version, root) =>
        sql"select node_id, childs, attrs, cont, creator_id from nodes where document_id = $a".as[DocumentRepository.NodeResult]
          .map(a => (root, a, version))
    }
    db.run(query.transactionally).map(kk => {
      val rootId = kk._1
      val nodes = kk._2.map(a => (a.nodeId, a)).toMap
      def materializeNode(id: UUID): model.data.Node = {
        val res = nodes(id)
        model.data.Node(id, res.content, res.attributes.asInstanceOf[JsObject], res.childs.map(materializeNode))
      }
      Server.InitResult(materializeNode(rootId), kk._3, nodes.mapValues(a => a.creatorId))
    })
  }




  def changes(userId: UUID, did: UUID, version: Int, changes: Seq[(model.transaction.Node, UUID, Seq[model.operation.Node.Diff])]): Future[Unit] = {
    val time = System.currentTimeMillis()
    val ops = changes.zipWithIndex.flatMap(p => {
      val c = p._1
      val v = version + p._2
      c._3.map(d => diffToQuery(userId, did, time, c._2, d)) :+ sqlu"insert into changes values ($did, ${c._2}, $v, $time, ${c._1})"
    }) :+ sqlu"update documents set current_version = ${version + changes.size}, last_updated_time = $time where document_id = $did"
    db.run(DBIO.seq(ops : _*).transactionally).map(_ => Unit)
  }

}
