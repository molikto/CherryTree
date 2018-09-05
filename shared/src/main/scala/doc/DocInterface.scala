package doc

import monix.reactive.Observable
import search.SearchInterface


trait DocInterface extends SearchInterface {

  def state: DocState
  def stateUpdates: Observable[DocUpdate]
}
