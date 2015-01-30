package actors

import actors.gcounter.{Print, SendUpdate, Increment}
import akka.actor.{ActorRef, Actor}
import crdts.HandOff
import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor._
import com.typesafe.config.ConfigFactory
import crdts.GCounter

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration


case class SmartUpdate(counter: HandOff.Node)

class SmartClient(server: ActorSelection) extends Actor {
  private var counter = HandOff.Node("client-" + UUID.randomUUID().toString, 1)

  override def receive: Receive = {
    case SmartUpdate(other) =>
      counter = counter.join(other)

    case Increment =>
      counter = counter.incr

    case SendUpdate =>
      server ! SmartUpdate(counter)

    case Print =>
      println(s"Counter for ${counter.id} is ${counter.value}.")
  }

}

object SmartClient {

  def apply()(implicit actorSystem: ActorSystem): ActorRef = SmartClient(FiniteDuration(100, TimeUnit.MILLISECONDS))
  def apply(interval: FiniteDuration)(implicit actorSystem: ActorSystem): ActorRef = {
    val server = actorSystem.actorSelection("akka.tcp://handoffcounterSystem@127.0.0.1:2552/user/smart-server")

    val client = actorSystem.actorOf(Props(classOf[SmartClient], server), UUID.randomUUID().toString)

    implicit val ec = actorSystem.dispatcher
    actorSystem.scheduler.schedule(interval, interval, client, SendUpdate)
    actorSystem.scheduler.schedule(interval, interval, client, Print)

    // Keep incrementing the counter for a while
    Future {
      (1 to 10) foreach { _ =>
        client ! Increment
        Thread.sleep(100)
      }

      println("Stopped incrementing the counter now .. ")
    }

    client
  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("handoffcounterSystem", ConfigFactory.load("client.conf"))
    SmartClient()
  }

}
