package server

import model._
import api._
import model.data.{Text, Unicode}
import model.ot.Rebased

import scala.collection.mutable
import scala.util.Random

class Server extends Api {

  // states, now in single thread fashion
  private var document = data.Node(data.Content.Paragraph(data.Paragraph(Seq(
    Text.Plain(Unicode("Plain text with some ")),
    Text.Strong(Seq(
      Text.Plain(Unicode("strong text and ")),
      Text.Code(Unicode("div")),
      Text.Plain(Unicode(" some code")),
    )),
    Text.Plain(Unicode(" and ")),
    Text.Emphasis(Seq(
      Text.Plain(Unicode("em text and ")),
      Text.Code(Unicode("html")),
      Text.Plain(Unicode(" some code"))
    )),
    Text.Plain(Unicode(". ")),
    Text.StrikeThrough(Seq(Text.Plain(Unicode("des text and ")),
      Text.Strong(Seq(Text.Plain(Unicode("strong text inside")))),
    )),
    Text.Plain(Unicode(" ")),
    Text.Link(Seq(Text.Plain(Unicode("link text and ")), Text.Code(Unicode("CODE INSIDE"))), Unicode("http:www.google.com")),
    Text.Plain(Unicode(" some latex ")),
    Text.LaTeX(Unicode("a + b + \\frac{c}{\\frac{b}{\\sqrt{2321312} + 2} + \\inf}")),
    Text.Plain(Unicode(" and some image ")),
    Text.Image(Seq.empty, Unicode("https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Comiclogo.svg/106px-Comiclogo.svg"))
  ))), Seq(data.Node.random()))
  private var changes = Seq.empty[transaction.Node]
  def version: Int = changes.size
  private val clients: mutable.Map[Authentication.Token, Int] = mutable.Map.empty
  private var debugHistoryDocuments = Seq(document)

  def debugDocument = document
  def debugChanges = changes

  override def init(token: Authentication.Token): Either[ApiError, ClientInit] = synchronized {
    // LATER sync mode back to client?
    val state = ClientInit(document, model.mode.Node.Content(Seq.empty, document.content.defaultNormalMode()), version)
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
