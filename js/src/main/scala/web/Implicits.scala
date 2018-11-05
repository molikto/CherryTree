package web

trait Implicits {

  implicit def monixImplicit = monix.execution.Scheduler.Implicits.global
}
