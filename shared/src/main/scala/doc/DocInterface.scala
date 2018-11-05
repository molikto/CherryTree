package doc

import api.NodeInfo
import monix.reactive.Observable
import search.SearchInterface

import scala.concurrent.Future


trait DocInterface extends SearchInterface {


  def state: DocState
  def stateUpdates: Observable[DocUpdate]

  def getNodeInfo(uuid: String): Future[Option[NodeInfo]]
}
