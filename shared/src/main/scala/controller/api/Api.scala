package controller.api

import model._

trait Api {


  /**
    * initialize current client state
    * each session has a unique token, we currently don't have a user concept, a user can have multi-session
    * but each one must have a unique token
    * if two connection shares a same token, then the client should count for client version inconsistencies
    * and then tell the user to login again something like this
    *
    *
    * the client version is used to track the version the user is in, so network errors don't cause duplicated
    * edits
    */
  def init(
    token: Authentication.Token
  ): Either[ApiError, ClientInit]


  /**
    * post a diff to the server
    */
  def change(
    authentication: Authentication.Token,
    version: Int,
    changes: Seq[transaction.Node]
  ): Either[ApiError, ClientUpdate]


  // LATER separate change and diff since
//  /**
//    *
//    */
//  def diffSince(
//    authentication: Authentication.Token,
//    version: Int
//  ): Either[ApiError, ClientUpdate]
}
