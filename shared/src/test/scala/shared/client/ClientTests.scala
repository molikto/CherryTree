
package shared.client

import shared._
import shared.api.{Api, Authentication}
import shared.data0.{Node, NodeOps}
import shared.server.CherryTreeServer
import utest._

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.{Random, Try}


object ClientTests extends TestSuite  {

  val tests = Tests {
    val server = new CherryTreeServer()
    val api = {
      val s = new TestApiAdapter(server)
      s[Api]
    }

    def serverDoc = server.debugDocument
    def serverChanges = server.debugChanges


    def cl(name: String) = ClientInitializer.init(api, Authentication.Token(name))

    'init - {
      Await.result(cl("client"), 1.seconds)
      Unit
    }

    'client - {
      val clients = (0 until 24).map(i => Await.result(cl("client" + i), 1.seconds))
      val client = clients.head

      def insertTop: Node.Transaction =
        Seq(NodeOps.insertContent(Seq.empty, 0, Random.nextLong().toString))
      def insertTop0(i: String): Node.Transaction =
        Seq(NodeOps.insertContent(Seq.empty, 0, i + " "))

      def waitAssertStateSyncBetweenClientsAndServer(): Unit = {
        val debug = false
        if (debug) {
          println("waiting for clients to commit")
        }
        while (clients.exists(a => a.hasUncommited)) {
          Thread.sleep(100)
        }
        if (debug) {
          println("clients updates finished")
        }
        clients.foreach(_.sync())
        while (clients.exists(a => a.updating)) {
          Thread.sleep(100)
        }
        if (debug) {
          println("Server doc " + serverDoc)
          clients.foreach(c => {
            println(c.debug_authentication + " " + c.debug_committed)
          })
        }
        clients.foreach(c => {
          assert(c.debug_committed == serverDoc)
        })
      }

      'oneWaySync - {
        client.change(insertTop)
        waitAssertStateSyncBetweenClientsAndServer()
        client.change(insertTop)
        client.change(insertTop)
        client.change(insertTop)
        client.change(insertTop)
        waitAssertStateSyncBetweenClientsAndServer()
      }

      'randomTwoClientTopInsertionsSync - {
        val count = 100
        for ((i, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
          clients(j).change(insertTop0(s"${('a' + j).toChar}$i"))
        }
        waitAssertStateSyncBetweenClientsAndServer()
        val str = serverDoc.content
        val ts = str.split(" ").filter(_.nonEmpty)
        assert(ts.size == count)
        def decreasing(a: Seq[Int]) = a.sliding(2, 1).forall(a => a.size < 2 || a.head > a(1))
        val vs = clients.indices.map(i => ts.filter(_.startsWith(('a' + i).toChar.toString)).map(_.drop(1).toInt).toSeq)
        assert(vs.forall(decreasing))
        assert(vs.flatten.toSet == (0 until count).toSet)
      }

      'randomSingleChangeTransactionSync - {
        val count = 100
        for ((_, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
          // sadly our tests is not one thread
          clients(j).synchronized {
            clients(j).change(Node.Ot.generateRandomTransaction(1, clients(j).state.get))
          }
        }
        waitAssertStateSyncBetweenClientsAndServer()
      }

      'randomTwoChangeTransactionSync - {
        val count = 100
        for ((_, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
          // sadly our tests is not one thread
          clients(j).synchronized {
            clients(j).change(Node.Ot.generateRandomTransaction(2, clients(j).state.get))
          }
        }
        waitAssertStateSyncBetweenClientsAndServer()
      }
    }
  }
}
