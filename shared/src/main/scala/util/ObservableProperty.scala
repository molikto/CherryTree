package util

import monix.execution.Cancelable
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.BehaviorSubject

class ObservableProperty[T](initial: T) extends Observable[T] {
  private val subject = BehaviorSubject(initial)
  private var t = initial

  def update(t: T): Unit = {
    if (t != this.t) {
      this.t = t
      subject.onNext(t)
    }
  }

  def modify(a: T => T): Unit = {
    update(a(get))
  }

  def get: T = t

  final override def unsafeSubscribeFn(subscriber: Subscriber[T]): Cancelable = subject.distinctUntilChanged.unsafeSubscribeFn(subscriber)
}

object ObservableProperty {
  def apply[T](t: T): ObservableProperty[T] = new ObservableProperty(t)
}
