package util


import scala.language.implicitConversions

trait ObservablePropertyImplicits {

  implicit def obsT2T[T](a: ObservableProperty[T]): T = a.get
}
