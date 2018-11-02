import java.nio.ByteBuffer

import api.ListResult
import com.mohiva.play.silhouette.api.AuthInfo
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.impl.providers.{OAuth1Info, OAuth2Info, OpenIDInfo}
import model.{Pickle, Unpickle}
import play.api.libs.json._
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}


package object repos {

  import utils.MyPostgresProfile.plainApi._

  type NodeResult = (String, Seq[String], JsValue, model.data.Content)

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




  implicit val nodeGetResult: GetResult[NodeResult] = GetResult[NodeResult](r => (r.<<, r.<<, r.<<(getJson), r.<<))

  implicit val listResultResult: GetResult[ListResult] = GetResult[ListResult](r => ListResult(r.<<, r.<<, r.<<, r.<<, r.<<))
}
