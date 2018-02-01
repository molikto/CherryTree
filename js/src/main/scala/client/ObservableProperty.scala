package client

import rxscalajs.Observable
import rxscalajs.facade.{BehaviorSubjectFacade, ObservableFacade}
import rxscalajs.subjects.BehaviorSubject



trait ObservableProperty[T] extends Observable[T] {
  def update(t: T)
  def get: T
}

private class ObservablePropertyImpl[T](var t: T) extends BehaviorSubject(new BehaviorSubjectFacade(t)) with ObservableProperty[T] {

  override def update(t: T): Unit = {
    this.next(t)
    this.t = t
  }

  override def get: T = t
}

object ObservableProperty {
  def apply[T](t: T): ObservableProperty[T] = new ObservablePropertyImpl(t)
}
