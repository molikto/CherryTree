import boopickle.{Encoder, PickleState, Pickler, UnpickleState}
import com.mohiva.play.silhouette.api.AuthInfo
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.impl.providers.{OAuth1Info, OAuth2Info, OpenIDInfo}
import util._

package object picklers {



//
//  implicit val authInfoPickler: Pickler[AuthInfo] = new Pickler[AuthInfo] {
//    override def pickle(obj: AuthInfo)(implicit state: PickleState): Unit = {
//      import state.enc._
//      obj match {
//        case a: OAuth1Info =>
//          writeInt(0)
//          writeString(a.secret)
//          writeString(a.token)
//        case a: OAuth2Info =>
//          writeInt(1)
//          writeString(a.accessToken)
//          writeString(a.tokenType.getOrElse(""))
//          writeInt(a.expiresIn.getOrElse(-1))
//          writeString(a.refreshToken.getOrElse(""))
//          writeStringMap(a.params.getOrElse(Map.empty))
//        case c: PasswordInfo =>
//          writeInt(2)
//          writeString(c.hasher)
//          writeString(c.password)
//          writeString(c.salt.getOrElse(""))
//        case o: OpenIDInfo =>
//          writeInt(3)
//          writeString(o.id)
//          writeStringMap(o.attributes)
//        case _ => throw new IllegalArgumentException("Not supported type")
//      }
//    }
//
//    override def unpickle(implicit state: UnpickleState): AuthInfo = {
//      import state.dec._
//      readInt match {
//        case 0 =>
//          OAuth1Info(readString, readString)
//        case 1 =>
//          OAuth2Info(
//            readString,
//            emptyToOption(readString),
//            negativeAsNone(readInt),
//            emptyToOption(readString),
//            emptyToOption(readStringMap))
//        case 2 =>
//          PasswordInfo(readString, readString, emptyToOption(readString))
//        case 3 =>
//          OpenIDInfo(readString, readStringMap)
//        case _ => throw new IllegalArgumentException("Not supported type")
//      }
//    }
//  }
}
