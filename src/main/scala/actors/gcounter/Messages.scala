package actors.gcounter

import crdts.GCounter

case object Print
case object Increment
case object SendUpdate
case class ReceiveUpdate(counter: GCounter[String])