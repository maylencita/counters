package actors

import akka.actor.{Props, ActorRef, ActorSystem, Actor}
import crdts.GCounter
import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import com.typesafe.config.ConfigFactory

class DummyServer extends Actor{
  private var counter = SimpleState("server", 0)

  override def receive: Receive = {
    case SimpleUpdate(other) =>
      counter = counter.copy(count = other)
      sender() ! SimpleUpdate(counter.count)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.count}.")
  }

}

object DummyServer{
  def apply()(implicit actorSystem: ActorSystem): ActorRef = Server(FiniteDuration(5, TimeUnit.SECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorOf(Props[DummyServer], "dummy-server")

    implicit val ec = actorSystem.dispatcher
    actorSystem.scheduler.schedule(interval, interval, server, Print)

    server
  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("dummycounterSystem", ConfigFactory.load("server.conf"))
    Server()
  }
}
