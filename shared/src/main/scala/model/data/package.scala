package model

import boopickle._
import model.range.IntRange

import scala.util.Random

package object data extends SpecialCharTrait {

  trait DataObject[T] {
    def random(): T = random(new Random())
    def random(r: Random): T
    val pickler: boopickle.Pickler[T]
  }
}
