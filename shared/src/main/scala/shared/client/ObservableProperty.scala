package shared.client

trait ObservableProperty[T] {
  def update(t: T)
  def get: T
  def subscribe(a: T => Unit): Subscription
}

trait Subscription {
  def dispose()
}

private class ObservablePropertyImpl[T](var t: T) extends ObservableProperty[T] {

  override def update(t: T): Unit = {
    this.t = t
    if (subscriber != null) subscriber(t)
  }

  override def get: T = t

  var subscriber: T => Unit = null
  override def subscribe(a: T => Unit): Subscription = {
    if (subscriber != null) {
      throw new IllegalStateException("Not supported yet")
    }
    subscriber = a
    subscriber(t)
    new Subscription {
      override def dispose(): Unit = subscriber = null
    }
  }
}

object ObservableProperty {
  def apply[T](t: T): ObservableProperty[T] = new ObservablePropertyImpl(t)
}
