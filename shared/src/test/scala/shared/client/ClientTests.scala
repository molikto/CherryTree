package shared.client

import shared._
import shared.data._
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
    }

    'client - {
      val clients = (0 until 24).map(i => Await.result(cl("client" + i), 1.seconds))
      val client = clients.head

      def insertTop = Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), Random.nextLong() + " ")))
      def insertTop0(i: String) = Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), i + " ")))

      def waitAssertStateSyncBetweenClientsAndServer(): Unit = {
        val debug = false
        clients.map(c => {
          val t = new Thread() {
            override def run(): Unit = {
              c.sync()
              while (c.hasUncommited || c.updating) {
                Thread.sleep(100)
              }
              if (debug) println(s"client ${c.debugCommited.authentication} updated")
              c.sync()
              while (c.hasUncommited || c.updating) {
                Thread.sleep(100)
              }
              Thread.sleep(100)
              c.sync()
              Thread.sleep(100)
              c.sync()
            }
          }
          t.start()
          t
        }).foreach(_.join())
        if (debug) {
          println("Server doc " + serverDoc.root)
          clients.foreach(c => {
            println(c.debugCommited.authentication + " " + c.debugCommited.document.root)
          })
        }
        clients.foreach(c => {
          assert(c.debugCommited.document == serverDoc)
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

      'randomTwoClientInsertionsSync - {
        val count = 2000
        for ((i, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
          clients(j).change(insertTop0(s"${('a' + j).toChar}$i"))
        }
        waitAssertStateSyncBetweenClientsAndServer()
        val str = serverDoc.root.content
        val ts = str.split(" ").filter(_.nonEmpty)
        assert(ts.size == count)
        def decreasing(a: Seq[Int]) = a.sliding(2, 1).forall(a => a.head > a(1))
        val vs = clients.indices.map(i => ts.filter(_.startsWith(('a' + i).toChar.toString)).map(_.drop(1).toInt).toSeq)
        assert(vs.forall(decreasing))
        assert(vs.flatten.toSet == (0 until count).toSet)
      }
    }
  }
}
