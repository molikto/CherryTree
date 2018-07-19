package doc

import monix.reactive.Observable


trait DocInterface {

  def state: DocState
  def stateUpdates: Observable[DocUpdate]
}
