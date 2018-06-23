package model

import boopickle._

import scala.util.Random

package object data {
  type Paragraph = Seq[Text]


  trait DataObject[T] {
    def generateRandom(): T = generateRandom(new Random())
    def generateRandom(random: Random): T
    val pickler: Pickler[T]
  }
}
