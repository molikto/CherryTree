
package client

import java.io.{Closeable, IOException}
import java.nio.ByteBuffer
import java.util.UUID

import command.{CommandInterface, CommandInterfaceAvailable, FindCommand, Part}
import command.Key.Grapheme
import command.Part.IdentifiedCommand
import doc.DocTransaction
import model._
import model.data.{EncodedSeq, Node, Unicode}
import model.ot.NodeOps
import monix.reactive.Observable
import server.Server
import utest._
import api._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Random, Try}


object ClientTests extends TestSuite  {


  model.debug_view = false
  model.debug_testing = true
  model.debug_model = true

  val tests = Tests {

    val documentId = UUID.randomUUID()

    case class TestUser(override val userId: UUID) extends Server.User {
      override def toCollabrator: Collaborator = Collaborator("", "", None)
    }
    val s = new server.Server[TestUser](Server.InitResult(Node.create("Test document"), 0, Map.empty)) {

    }

    val random = new Random()

    def serverDoc = s.debugDocument
    def serverChanges = s.debugChanges


    def cl(name: String): Future[Client] = ClientInitializer.init(new Api {
      override val localStorage: LocalStorage = new LocalStorage {

        val map = mutable.Map[String, String]()

        override def set(key: String, str: String): Unit = map.update(key, str)

        override def remove(key: String): Unit = map.remove(key)

        override def get(key: String): Option[String] = map.get(key)
      }

      override def requestBytes(path: String, content: ByteBuffer, method: String): Future[ByteBuffer] = {
        if (path == s"/document/${documentId}/init") {
          println("init called")
          val res = s.init(TestUser(UUID.randomUUID()), 100)
          Future.successful(Pickle.intoBytes(res))
        } else if (path == s"/document/${documentId}/changes") {
          val res = s.change(TestUser(UUID.randomUUID()), Unpickle[ChangeRequest](api.changeRequest).fromBytes(content), 100).get
          if (random.nextInt(10) == 7) {
            // network failures
            Future.failed(new IOException())
          } else {
            Future.successful(Pickle.intoBytes[ChangeResponse](res))
          }
        } else {
          println(path)
          ???
        }
      }

      override def setupWebSocket(path: String): (Closeable, Observable[Either[String, Throwable]]) = {
        (new Closeable {
          override def close(): Unit = {}
        }, Observable.never)
      }
    }, documentId, None)

    'init - {
      Await.result(cl("controller"), 5.seconds)
      Unit
    }

    'client - {
      val clients = (0 until 5).map(i => Await.result(cl("controller" + i), 5.seconds))
      val client = clients.head

      def insertTop: transaction.Node =
        Seq(NodeOps.insertContent(Seq.empty, 0, Random.nextLong().toString))
      def insertTop0(i: String): transaction.Node =
        Seq(NodeOps.insertContent(Seq.empty, 0, i + " "))

        def waitAssertStateSyncBetweenClientsAndServer(): Unit = {
          val debug = false
          if (debug) {
            println("waiting for clients to commit")
          }
          while (clients.exists(a => {
            val res = a.hasUncommited
            if (res) {
              val sycing = a.sync()
              println(s"not finished ${a.debug_blockReason} $sycing")
            }
            res
          })) {
            Thread.sleep(100)
          }
          if (debug) {
            println("clients changes finished")
          }
          while (clients.exists(a => a.debug_committedVersion != serverChanges.size)) {
            println(s"not updated ${clients.forall(_.sync())}")
            Thread.sleep(100)
          }
          if (debug) {
            println("Server doc " + serverDoc)
            clients.foreach(c => {
              println(c.debug_committedVersion  + " " + c.debug_committed)
            })
          }
          clients.foreach(c => {
            val client = c.debug_committed
            val sd = serverDoc
            assert(client == sd)
          })
        }

  //      'oneWaySync - {
  //        client.change(insertTop)
  //        waitAssertStateSyncBetweenClientsAndServer()
  //        client.change(insertTop)
  //        client.change(insertTop)
  //        client.change(insertTop)
  //        client.change(insertTop)
  //        waitAssertStateSyncBetweenClientsAndServer()
  //      }
  //
  //      'randomTwoClientTopInsertionsSync - {
  //        val count = 100
  //        for ((i, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
  //          clients(j).change(insertTop0(s"${('a' + j).toChar}$i"))
  //        }
  //        waitAssertStateSyncBetweenClientsAndServer()
  //        val str = serverDoc.content.asInstanceOf[data.Content.Code].unicode.toString
  //        val ts = str.split(" ").filter(_.nonEmpty)
  //        assert(ts.size == count)
  //        def decreasing(a: Seq[Int]) = a.sliding(2, 1).forall(a => a.size < 2 || a.head > a(1))
  //        val vs = clients.indices.map(i => ts.filter(_.startsWith(('a' + i).toChar.toString)).map(_.drop(1).toInt).toSeq)
  //        assert(vs.forall(decreasing))
  //        assert(vs.flatten.toSet == (0 until count).toSet)
  //      }

        'randomSingleChangeTransactionSync - {
          val count = 1000
          for ((i, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
            // sadly our tests is not one thread
            val c = clients(j)
            c.synchronized {
              c.debug_unmarkTempDisableMode()
              c.localChange(DocTransaction(operation.Node.randomTransaction(1, clients(j).state.node, random), None))
            }
          }
          waitAssertStateSyncBetweenClientsAndServer()
        }

        'randomTwoChangeTransactionSync - {
          val count = 1000
          for ((_, j) <- (0 until count).map(a => (a, random.nextInt(clients.size)))) {
            // sadly our tests is not one thread
            val c = clients(j)
            c.synchronized {
              c.debug_unmarkTempDisableMode()
              c.localChange(DocTransaction(operation.Node.randomTransaction(2, clients(j).state.node, random), None))
            }
          }
          waitAssertStateSyncBetweenClientsAndServer()
        }

        'performanceTwoChangeTransactionSync - {
          val random = new Random(21321321)
          val count = 100
          for ((_, j) <- (0 until count).map(a => (a, random.nextInt(clients.size)))) {
            // sadly our tests is not one thread
            val c = clients(j)
            c.synchronized {
              c.debug_unmarkTempDisableMode()
              c.localChange(DocTransaction(operation.Node.randomTransaction(2, clients(j).state.node, random), None))
            }
          }
          waitAssertStateSyncBetweenClientsAndServer()
        }

      'humanLikeTest - {

        /**
          * current limitation: only commands with out needs
          */
        def performAHumanAction(ith: Int, c: Client) = {
          if (c.state.isRichInsert) {
            c.debug_unmarkTempDisableMode()
            c.onInsertRichTextAndViewUpdated(0, 0, Unicode(Random.nextInt().toString), false, -1, false)
          }
          val avs = c.commands.commands.filter(a => a.available(c.state, c.commands))
          if (avs.nonEmpty) {
            val ccc = avs(Random.nextInt(avs.size - 1))
            println(s"$ith performing ${ccc.description}")
            val motion: command.Motion = if (ccc.needsMotion) {
              val ms = c.commands.commands.filter(a => a.isInstanceOf[command.Motion] && a.available(c.state, new CommandInterfaceAvailable {
                override def lastFindCommand: Option[(FindCommand, Unicode)] = c.commands.lastFindCommand

                override protected def commandBuffer: Seq[Part] = Seq(IdentifiedCommand(None, ccc, Seq.empty))
              })
              ).map(_.asInstanceOf[command.Motion])
              ms(Random.nextInt(ms.size - 1))
            } else {
              null
            }
            val chhh = if (ccc.needsChar || (motion != null && motion.asInstanceOf[command.Command].needsChar)) {
              val chars = (('a' to 'z') ++ ('0' to '9')).toSeq
              Some(Unicode(String.valueOf(chars(Random.nextInt(chars.size - 1)))))
            } else {
              None
            }
            val trans = ccc.action(
              c.state,
              Random.nextInt(10),
              c.commands,
              if (Random.nextBoolean()) None else ccc.keys.headOption,
              chhh, Option(motion))
            c.debug_unmarkTempDisableMode()
            c.localChange(trans)
          }
        }

        for (c <- clients) {
          import monix.execution.Scheduler.Implicits.global
          c.searchState.doOnNext(state => {
            if (c.searchHandler.searchState.get.searching.nonEmpty) {
              c.synchronized {
//                println("committing search")
//                c.searchHandler.updateConstructingSearchTerm(random.nextInt(100).toString)
//                c.searchHandler.commitSearching(true)
              }
            }
          }).subscribe()
        }
        for (j <- 0 until 1000) {
          for (c <- clients) {
            for (i <- 0 until Random.nextInt(5)) {
              c.synchronized {
                performAHumanAction(j, c)
              }
            }
          }
        }
        waitAssertStateSyncBetweenClientsAndServer()
      }
    }
  }
}
