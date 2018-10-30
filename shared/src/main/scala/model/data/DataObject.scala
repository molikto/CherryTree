package model.data

import play.api.libs.json.Format

import scala.util.Random

trait DataObject[T] {
  def random(): T = random(new Random())
  def random(r: Random): T
  val jsonFormat: Format[T]
}

