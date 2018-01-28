package shared

import scala.util.Random

package object data extends ChangeImplicits  {

  import boopickle.Default._
  implicit val datePickler = transformPickler((t: Long) => new java.util.Date(t))(_.getTime)


  def generateNewRandomUniqueId(): String = Random.nextString(12)
}
