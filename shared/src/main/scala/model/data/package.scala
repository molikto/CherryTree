package model

import boopickle._

import scala.util.Random

package object data {
  type Paragraph = Seq[Text]


  trait DataObject[T] {
    def random(): T = random(new Random())
    def random(random: Random): T
    val pickler: boopickle.Pickler[T]
  }
}
