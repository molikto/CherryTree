package shared.data

import boopickle.CompositePickler
import boopickle.Default.compositePickler


trait ChangeImplicits {

  implicit val changeNodePickler: CompositePickler[Change.Node] =
    compositePickler[Change.Node]
      .addConcreteType[Change.Node.Delete]
      .addConcreteType[Change.Node.Insert]

  implicit val changeContentPickler: CompositePickler[Change.Content] =
    compositePickler[Change.Content]
      .addConcreteType[Change.Content.Insert]
      .addConcreteType[Change.Content.Delete]

  implicit val changePickler: CompositePickler[Change] =
    compositePickler[Change]
      .join[Change.Node](changeNodePickler)
      .join[Change.Content](changeContentPickler)
}

