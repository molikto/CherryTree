package server

import model._
import api._
import model.data.{Text, Unicode}
import model.ot.Rebased

import scala.collection.mutable
import scala.util.Random

class Server extends Api {

  // states, now in single thread fashion
  private var document = data.Node(data.Content.Rich(data.Rich(Seq(

    Text.Plain(Unicode("😀💩👮🏿‍♀️👩‍👩‍👧‍👧🇫🇮 some latex ")),
    Text.Code(Unicode( "\u0628" + "\u064e" + "\u064a"  + "😀💩👮🏿‍♀️👩‍👩‍👧‍👧🇫🇮 some latex ")),
    Text.LaTeX(Unicode("a + b + \\frac{c}{\\frac{b}{\\sqrt{2321312} + 2} + \\inf}")),
    Text.Plain(Unicode(" and <b>should be escaped</b> spaces   some image " + "\u0628" + "\u064e" + "\u064a" +
      "\u0652" + "\u067a" + "\u064f ")),
    Text.Image(Unicode("https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Comiclogo.svg/106px-Comiclogo.svg")),
    Text.Strong(Seq(
      Text.Plain(Unicode("strong text and ")),
      Text.Code(Unicode("div")),

      Text.Plain(Unicode(" some code")),
    )),
    Text.Plain(Unicode(" plain text with some ")),
    Text.Plain(Unicode(" and ")),
    Text.Emphasis(Seq(
      Text.Plain(Unicode("em text and ")),
      Text.Code(Unicode("html")),
      Text.Plain(Unicode(" some code"))
    )),
    Text.Plain(Unicode(". ")),
    Text.StrikeThrough(Seq(Text.Emphasis(Seq(Text.Plain(Unicode("des text and ")))),
      Text.Strong(Seq(Text.Plain(Unicode("strong text inside")))),
    )),
    Text.Plain(Unicode(" ")),
    Text.Link(Seq(Text.Plain(Unicode("link text and ")), Text.Code(Unicode("CODE INSIDE"))), Unicode("http:www.google.com"))
  ))), Seq(data.Node(data.Content.Code(Unicode.random(), ""), Seq.empty)))
  private var changes = Seq.empty[transaction.Node]
  def version: Int = changes.size
  private val clients: mutable.Map[Authentication.Token, Int] = mutable.Map.empty
  private var debugHistoryDocuments = Seq(document)

  def debugDocument = document
  def debugChanges = changes

  override def init(token: Authentication.Token): Either[ApiError, ClientInit] = synchronized {
    // LATER sync mode back to client?
    val state = ClientInit(document, model.data.Node.defaultNormalMode(document, cursor.Node.root), version)
    clients.update(token, version)
    Right(state)
  }

  /**
    * @return unread transactions
    */
//  private def checkReadStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[transaction.Node]] = synchronized {
//    clients.get(authentication) match {
//      case None =>
//        Left(ApiError.InvalidToken)
//      case Some(cached) =>
//        Right(changes.drop(version))
//    }
//  }


  /**
    *
    * @param version current client version
    * @return
    */
  private def checkWriteStateConsistency(authentication: Authentication.Token, version: Int): ErrorT[Seq[transaction.Node]] = synchronized {
    clients.get(authentication) match {
      case None =>
        Left(ApiError.InvalidToken)
      case Some(cached) =>
        val diff = version - cached
         if (diff < 0) {
           Left(ApiError.ClientVersionIsOlderThanServerCache)
        } else if (diff > 0) {
           Left(ApiError.ClientVersionIsHigherThanServerCache)
        } else {
          Right(changes.drop(version))
        }
    }
  }

  override def change(authentication: Authentication.Token, clientVersion: Int, ts: Seq[transaction.Node], mode: Option[model.mode.Node], debugClientDoc: data.Node): ErrorT[ClientUpdate] = synchronized {
    checkWriteStateConsistency(authentication, clientVersion).map { ws =>
      try {
        if (debugModel) {
          assert(debugClientDoc == debugHistoryDocuments(clientVersion))
        }
        val Rebased(conflicts, (wws, transformed)) = ot.Node.rebaseT(ws.flatten, ts)
        var debugTopDocument = document
        document = operation.Node.applyT(transformed, document)
        changes = changes ++ transformed
        for (t <- transformed) {
          debugTopDocument = operation.Node.apply(t, debugTopDocument)
          debugHistoryDocuments = debugHistoryDocuments :+ debugTopDocument
        }
        if (debugModel) {
          assert(operation.Node.apply(wws, operation.Node.applyT(ts, debugHistoryDocuments(clientVersion))) == document)
        }
        clients.update(authentication, version)
        // LATER don't accept conflicting items
        ClientUpdate(ws, ts.size, version)
      } catch {
        case e: Throwable => throw e
      }
    }
  }
}
