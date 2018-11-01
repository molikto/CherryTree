package repos

import java.nio.ByteBuffer
import java.util.UUID

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
import play.api.libs.json.{Format, JsObject, JsResult, JsValue, Json}
import model._
import api._
import model.data.Content
import model.operation.Node
import model.transaction.Node

object DocumentRepository {

  type NodeResult = (String, Seq[String], Map[String, String], model.data.Content)
}

@Singleton
class DocumentRepository@Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
  (implicit ex: ExecutionContext) extends  DatabaseAccessing {


  import utils.MyPostgresProfile.plainApi._
  import DocumentRepository._


  private implicit val attrsGetResult: GetResult[Map[String, String]] = {
    val a: GetResult[Array[Byte]] = implicitly
    a.andThen(j => Unpickle[Map[String, String]](implicitly).fromBytes(ByteBuffer.wrap(j))(implicitly))
  }

  private implicit val attrsSet: SetParameter[Map[String, String]] = (v1: Map[String, String], v2: PositionedParameters) => {
    val prev: SetParameter[Array[Byte]] = implicitly
    prev(Pickle.intoBytes(v1).array(), v2)
  }


  private implicit val contGetResult: GetResult[model.data.Content] = {
    val a: GetResult[Array[Byte]] = implicitly
    a.andThen(j => Unpickle[model.data.Content](implicitly).fromBytes(ByteBuffer.wrap(j))(implicitly))
  }

  private implicit val contSet: SetParameter[model.data.Content] = (v1: model.data.Content, v2: PositionedParameters) => {
    val prev: SetParameter[Array[Byte]] = implicitly
    prev(Pickle.intoBytes(v1).array(), v2)
  }


  private implicit val transGetResult: GetResult[model.transaction.Node] = {
    val a: GetResult[Array[Byte]] = implicitly
    a.andThen(j => Unpickle[model.transaction.Node](implicitly).fromBytes(ByteBuffer.wrap(j))(implicitly))
  }

  private implicit val transSet: SetParameter[model.transaction.Node] = (v1: model.transaction.Node, v2: PositionedParameters) => {
    val prev: SetParameter[Array[Byte]] = implicitly
    prev(Pickle.intoBytes(v1).array(), v2)
  }



  private implicit val nodeGetResult: GetResult[NodeResult] = GetResult[NodeResult](r => (r.<<, r.<<, r.<<(attrsGetResult), r.<<(contGetResult)))


  def init(a: String): Future[(model.data.Node, Int)] = {
    val query = sql"select current_version, root_node_id from documents where document_id = $a".as[(Int, String)].head.flatMap {
      case (version, root) =>
        sql"select node_id, childs, attrs, cont from nodes where document_id = $a".as[NodeResult]
          .map(a => (root, a, version))
    }
    db.run(query.transactionally).map(kk => {
      val rootId = kk._1
      val nodes = kk._2.map(a => (a._1, a)).toMap
      def materializeNode(id: String): model.data.Node = {
        val res = nodes(id)
        model.data.Node(id, res._4, res._3, res._2.map(materializeNode))
      }
      (materializeNode(rootId), kk._3)
    })
  }




  def changes(did: String, version: Int, changes: Seq[(model.transaction.Node, Seq[model.operation.Node.Diff])]): Future[Unit] = {
    val ops = changes.zipWithIndex.flatMap(p => {
      val c = p._1
      val v = version + p._2
      c._2.map {
        case model.operation.Node.Diff.Insert(id, childs, attributes, content) =>
          sqlu"insert into nodes values ($did, $id, $childs, $attributes, $content)"
        case model.operation.Node.Diff.Update(id, childs, attributes, content) =>
          sqlu"update nodes set childs = $childs, attrs = $attributes, cont = $content where node_id = $id"
        case model.operation.Node.Diff.Delete(id) =>
          sqlu"delete from nodes where node_id = $id"
      } :+ sqlu"insert into changes values ($did, $v, ${c._1})"
    }) :+ sqlu"update documents set current_version = ${version + changes.size} where document_id = $did"
    db.run(DBIO.seq(ops : _*).transactionally).map(_ => Unit)
  }

}
