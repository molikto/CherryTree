package shared.data

import scala.util.Random


trait IdGenerator {

  def newId(): String = Random.nextString(12)
}
