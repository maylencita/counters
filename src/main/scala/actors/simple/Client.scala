package actors.simple

import java.util.UUID
import java.util.concurrent.TimeUnit

import actors.gcounter.{Increment, Print, SendUpdate}
import akka.actor.{Actor, ActorRef, _}
import com.typesafe.config.ConfigFactory

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

case class SimpleUpdate(count: Int)
case class SimpleState(id: String, count:Int)

class Client(server: ActorSelection) extends Actor {
  private var counter = SimpleState("client-" + self.path.name, 0)

  override def receive: Receive = {
    case SimpleUpdate(other) =>
      counter = counter.copy(count = other)

    case Increment =>
      counter = counter.copy(count = counter.count + 1)

    case SendUpdate =>
      server ! SimpleUpdate(counter.count)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.count}.")
  }

}

object Client {

  def apply()(implicit actorSystem: ActorSystem): ActorRef = Client(FiniteDuration(100, TimeUnit.MILLISECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorSelection("akka.tcp://dummycounterSystem@127.0.0.1:2552/user/dummy-server")

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
