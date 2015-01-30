package actors

import java.util.UUID

import akka.actor.{ActorRef, Actor}
import crdts.GCounter

case object Tick

/**
 *
 */
class Client(server: ActorRef) extends Actor {

  private var counter = GCounter[String]("client-" + UUID.randomUUID().toString)

  override def receive: Receive = {
    case Update(other) =>
      counter = counter.merge(other)

    case Tick =>
      val increment = (Math.random() * 10).asInstanceOf[Int]
      counter = counter.increment(increment)
      server ! Update(counter)

      println(s"Counter for ${counter.id} is ${counter.get}.")
  }

}

object Client {



}
