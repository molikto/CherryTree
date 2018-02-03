package shared.util

trait ObservablePropertyImplicits {

  implicit def obsT2T[T](a: ObservableProperty[T]): T = a.get
}
