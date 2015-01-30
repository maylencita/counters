package actors.simple

import java.util.concurrent.TimeUnit

import actors.gcounter.Print
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration.FiniteDuration

class Server extends Actor{
  private var counter = SimpleState("server", 0)

  override def receive: Receive = {
    case SimpleUpdate(other) =>
      counter = counter.copy(count = other)
      sender() ! SimpleUpdate(counter.count)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.count}.")
  }

}

object Server {
  def apply()(implicit actorSystem: ActorSystem): ActorRef = Server(FiniteDuration(5, TimeUnit.SECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorOf(Props[Server], "dummy-server")

    implicit val ec = actorSystem.dispatcher
    actorSystem.scheduler.schedule(interval, interval, server, Print)

    server
  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("dummycounterSystem", ConfigFactory.load("server.conf"))
    Server()
  }
}
