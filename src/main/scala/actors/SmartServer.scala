package actors

import actors.gcounter.Print
import akka.actor.{Props, ActorRef, ActorSystem, Actor}
import crdts.HandOff
import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import com.typesafe.config.ConfigFactory

class SmartServer extends Actor{
  private var counter = HandOff.Node("server", 0)

  override def receive: Receive = {
    case SmartUpdate(other) =>
      counter = counter.join(other)
      sender() ! SmartUpdate(counter)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.value}.")
  }

}

object SmartServer{
  def apply()(implicit actorSystem: ActorSystem): ActorRef = SmartServer(FiniteDuration(5, TimeUnit.SECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorOf(Props[SmartServer], "smart-server")

    implicit val ec = actorSystem.dispatcher
    actorSystem.scheduler.schedule(interval, interval, server, Print)

    server
  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("handoffcounterSystem", ConfigFactory.load("server.conf"))
    SmartServer()
  }
}
