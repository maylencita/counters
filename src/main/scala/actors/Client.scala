package actors

import java.util.UUID

import akka.actor.{ActorRef, Actor}
import crdts.GCounter

/**
 *
 */
class Client(server: ActorRef) extends Actor {

  private var counter = GCounter[String]("client-" + UUID.randomUUID().toString)

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



}
