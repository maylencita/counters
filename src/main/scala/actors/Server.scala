package actors

import java.util.concurrent.TimeUnit

import akka.actor.{Props, ActorRef, ActorSystem, Actor}
import crdts.GCounter

import scala.concurrent.duration.FiniteDuration

/**
 *
 */
class Server extends Actor {

  private var counter = GCounter[String]("server")

  override def receive: Receive = {
    case ReceiveUpdate(other) =>
      counter = counter.merge(other)
      sender() ! ReceiveUpdate(counter)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.get}.")
  }

}

object Server {
  def apply()(implicit actorSystem: ActorSystem): ActorRef = Server(FiniteDuration(5, TimeUnit.SECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorOf(Props[Server])

    implicit val ec = actorSystem.dispatcher
    actorSystem.scheduler.schedule(interval, interval, server, Print)

    server
  }
}
