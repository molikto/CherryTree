package model.data

import scala.util.Random

trait DataObject[T] {
  def random(): T = random(new Random())
  def random(r: Random): T
  val pickler: boopickle.Pickler[T]
}

