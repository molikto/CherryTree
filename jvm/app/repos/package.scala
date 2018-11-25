import java.nio.ByteBuffer
import java.util.UUID

import api.{Collaborator, ListResult, NodeInfo, PermissionLevel}
import com.mohiva.play.silhouette.api.AuthInfo
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.impl.providers.{OAuth1Info, OAuth2Info, OpenIDInfo}
import model.{Pickle, Unpickle}
import play.api.libs.json._
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}


package object repos {

  import utils.MyPostgresProfile.plainApi._


  implicit val passwordInfoFormat = Json.format[PasswordInfo]
  implicit val oauth1InfoFormat = Json.format[OAuth1Info]
  implicit val oauth2InfoFormat = Json.format[OAuth2Info]
  implicit val openIdInfoFormat = Json.format[OpenIDInfo]
  implicit val authInfoFormat: Format[AuthInfo] = new Format[AuthInfo] {
    override def reads(j: JsValue): JsResult[AuthInfo] = {
      val keys = j.asInstanceOf[JsObject].keys
      if (keys.contains("hasher")) {
        passwordInfoFormat.reads(j)
      } else if (keys.contains("accessToken")) {
        oauth2InfoFormat.reads(j)
      } else if (keys.contains("secret")) {
        oauth1InfoFormat.reads(j)
      } else {
        openIdInfoFormat.reads(j)
      }
    }

    override def writes(o: AuthInfo): JsValue = o match {
      case p: PasswordInfo => passwordInfoFormat.writes(p)
      case p: OAuth1Info => oauth1InfoFormat.writes(p)
      case p: OAuth2Info => oauth2InfoFormat.writes(p)
      case p: OpenIDInfo => openIdInfoFormat.writes(p)
      case _ => throw new IllegalStateException("Not supported")
    }
  }

  implicit val authInfoGetResult: GetResult[AuthInfo] = {
    val a: GetResult[JsValue] = implicitly
    a.andThen(j => authInfoFormat.reads(j).get)
  }

  implicit val authInfoSet: SetParameter[AuthInfo] = (v1: AuthInfo, v2: PositionedParameters) => {
    val prev: SetParameter[JsValue] = implicitly
    prev(authInfoFormat.writes(v1), v2)
  }



  implicit val contentGetResult: GetResult[model.data.Content] = {
    val a: GetResult[JsValue] = implicitly
    a.andThen(j => model.format_Content.reads(j).get)
  }

  implicit val contentSet: SetParameter[model.data.Content] = (v1: model.data.Content, v2: PositionedParameters) => {
    val prev: SetParameter[JsValue] = implicitly
    prev(model.format_Content.writes(v1), v2)
  }




  implicit val transGetResult: GetResult[model.transaction.Node] = {
    val a: GetResult[Array[Byte]] = implicitly
    a.andThen(j => Unpickle[model.transaction.Node](implicitly).fromBytes(ByteBuffer.wrap(j))(implicitly))
  }

  implicit val transSet: SetParameter[model.transaction.Node] = (v1: model.transaction.Node, v2: PositionedParameters) => {
    val prev: SetParameter[Array[Byte]] = implicitly
    prev(Pickle.intoBytes(v1).array(), v2)
  }




  implicit val nodeGetResult: GetResult[DocumentRepository.NodeResult] = GetResult[DocumentRepository.NodeResult](r => DocumentRepository.NodeResult(r.<<, r.<<, r.<<(getJson), r.<<, r.<<))

  implicit val listResultResult: GetResult[ListResult] = GetResult[ListResult](r => ListResult(r.<<, r.<<, r.<<, r.<<, Collaborator(r.<<, r.<<, r.<<), r.<<))


  implicit val nodeInfoResult: GetResult[NodeInfo] = GetResult[NodeInfo](r => NodeInfo(r.<<, r.<<, Collaborator(r.<<, r.<<, r.<<)))

  def createDocumentQuery(userId: UUID, node: model.data.Node, time: Long) = {
    val documentId = UUID.randomUUID()
    val createDocument =
      sqlu"insert into documents values ($documentId, ${node.uuid}, $time, $time, ${0})"
    val createPermission =
      sqlu"insert into permissions values ($userId, $documentId, ${PermissionLevel.Owner})"
    Seq(createDocument, createPermission) ++ model.operation.Node.createInsert(node).map(d => diffToQuery(userId, documentId, time, UUID.randomUUID(), d))
  }

  def diffToQuery(userId: UUID, did: UUID, time: Long, cid: UUID, a: model.operation.Node.Diff) = a match {
    case model.operation.Node.Diff.Insert(id, childs, attributes, content) =>
      sqlu"insert into nodes values ($did, $id, $time, $time, $childs, $attributes, $content, $userId)"
    case model.operation.Node.Diff.Update(id, childs, attributes, content) =>
      sqlu"update nodes set childs = $childs, attrs = $attributes, cont = $content, last_updated_time = $time where node_id = $id"
    case model.operation.Node.Diff.Delete(id) =>
      sqlu"delete from nodes where node_id = $id"
  }
}
