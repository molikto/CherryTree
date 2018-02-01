package shared.client

trait ObservableProperty[T] {
  def update(t: T)
  def get: T
}

private class ObservablePropertyImpl[T](var t: T) extends ObservableProperty[T] {

  override def update(t: T): Unit = {
    this.t = t
  }

  override def get: T = t
}

object ObservableProperty {
  def apply[T](t: T): ObservableProperty[T] = new ObservablePropertyImpl(t)
}
