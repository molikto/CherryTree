package model

import boopickle._
import model.range.IntRange

import scala.util.Random

package object data {
  type Paragraph = Seq[Text]

  trait DataObject[T] {
    def random(): T = random(new Random())
    def random(r: Random): T
    val pickler: boopickle.Pickler[T]
  }
}
