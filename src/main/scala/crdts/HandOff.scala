package crdts

import scala.collection.SortedMap

object HandOff {

  type ID = String
  type Tier = String
  type Clock = Int
  type Value = Int

  case class Node(
      id: ID,
      tier: Tier,
      value: Value = 0,
      below: Int = 0,
      sck: Clock = 0,
      dck: Clock = 0,
      slots: SortedMap[ID, (Clock, Clock)],
      tokens: SortedMap[(ID, ID), ((Clock, Clock), Value)],
      values: SortedMap[ID, Value]) {

    def incr: Node = copy(
      value = value + 1,
      values = values + (id -> (valueOf(id) + 1)))

    def join(other: Node): Node =
      if (id == other.id) this
      else Ops.list.foldLeft(this) {
        case (acc, op) => op(acc, other)
      }

    def valueOf(id: String): Value = values.getOrElse(id, 0)
  }

  object Ops {

    val list: List[(Node, Node) => Node] = List(
      fillSlots,
      discardSlot,
      mergeVectors,
      aggregate,
      discardTokens,
      createToken,
      cacheTokens)

    def fillSlots(a: Node, b: Node) = {
      val S = for {
        ((src, dst), ((sck, dck), n)) <- b.tokens
        if dst == a.id && a.slots.get(src).fold(false)((sck, dck) ==)
      } yield (src, n)
      a.copy(
        values = a.values + (a.id -> (a.valueOf(a.id) + S.map(_._2).sum)),
        slots = a.slots -- S.map(_._1))
    }

    def discardSlot(a: Node, b: Node) =
      if (a.slots.get(b.id).fold(false) { case (sck, _) => b.sck > sck })
        a.copy(slots = a.slots - b.id)
      else a

    def mergeVectors(a: Node, b: Node) = a

    def aggregate(a: Node, b: Node) = a

    def discardTokens(a: Node, b: Node) = a

    def createToken(a: Node, b: Node) = a

    def cacheTokens(a: Node, b: Node) = a
  }
}
