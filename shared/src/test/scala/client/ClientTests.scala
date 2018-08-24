
package client

import api.{Api, Authentication}
import doc.DocTransaction
import model._
import model.data.{Node, Unicode}
import model.ot.NodeOps
import server.Server
import utest._

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.{Random, Try}


object ClientTests extends TestSuite  {


  val tests = Tests {
    val s = new server.Server() {
      override def debugSave(a: String, bs: Array[Byte]): Unit = {
      }

      override def debugLoad(a: String): Array[Byte] = Array()
    }
    val api = {
      val k = new TestAutowireClient(s)
      k[Api]
    }

    val random = new Random()

    def serverDoc = s.debugDocument
    def serverChanges = s.debugChanges


    def cl(name: String) = ClientInitializer.init(api, Authentication.Token(name))

    'init - {
      Await.result(cl("controller"), 1.seconds)
      Unit
    }

    'client - {
      val clients = (0 until 5).map(i => Await.result(cl("controller" + i), 1.seconds))
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
            println(c.debug_authentication + " " + c.debug_committedVersion  + " " + c.debug_committed)
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
        for ((_, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
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
            try {
              c.onInsertRichTextAndViewUpdated(0, 0, Unicode(Random.nextInt().toString), -1)
            } catch {
              case e: Throwable =>
                println("action " + ith)
                println(c.state)
                throw e
            }
          }
          val avs = c.commands.filter(a => a.available(c.state, c) && !a.needsStuff)
          if (avs.nonEmpty) {
            val ccc = avs(Random.nextInt(avs.size - 1))
            try {
              val trans = ccc.action(
                c.state,
                Random.nextInt(10),
                c,
                if (Random.nextBoolean()) None else ccc.keys.headOption, None, None)
              try {
                c.debug_unmarkTempDisableMode()
                c.localChange(trans)
              } catch {
                case e: Throwable =>
                  println("action " + ith)
                  println(c.state)
                  println(ccc.description)
                  println(trans)
                  throw e
              }
            } catch {
              case e: Throwable =>
                println(c.state)
                println(ccc.description)
                throw e
            }
          }
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
