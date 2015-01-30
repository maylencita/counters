package actors.gcounter

import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor._
import com.typesafe.config.ConfigFactory
import crdts.GCounter

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/**
 *
 */
class Client(server: ActorSelection) extends Actor {

  private var counter = GCounter[String]("client-" + self.path.name)

  override def receive: Receive = {
    case ReceiveUpdate(other) =>
      counter = counter.merge(other)

    case Increment =>
      val increment = (Math.random() * 10).asInstanceOf[Int]
      counter = counter.increment(increment)

    case SendUpdate =>
      server ! ReceiveUpdate(counter)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.get}.")
  }

}

object Client {

  def apply()(implicit actorSystem: ActorSystem): ActorRef = Client(FiniteDuration(100, TimeUnit.MILLISECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorSelection("akka.tcp://gcounterSystem@127.0.0.1:2552/user/server")

    val client = actorSystem.actorOf(Props(classOf[Client], server), UUID.randomUUID().toString)

    implicit val ec = actorSystem.dispatcher
    actorSystem.scheduler.schedule(interval, interval, client, SendUpdate)
    actorSystem.scheduler.schedule(interval, interval, client, Print)

    // Keep incrementing the counter for a while
    Future {
      (1 to 100) foreach { _ =>
        client ! Increment
        Thread.sleep(100)
      }

      println("Stopped incrementing the counter now .. ")
    }

    client
  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("gcounterSystem", ConfigFactory.load("client.conf"))
    Client()
  }

}
