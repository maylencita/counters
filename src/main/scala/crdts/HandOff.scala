package crdts

import scala.collection.SortedMap

object HandOff {

  private type ID = String
  private type Tier = String
  private type Clock = Int
  private type Value = Int

  case class Node(
      id: ID,
      tier: Tier,
      value: Value = 0,
      below: Int = 0,
      sck: Clock = 0,
      dck: Clock = 0,
      slots: SortedMap[ID, (Clock, Clock)] = SortedMap.empty,
      tokens: SortedMap[(ID, ID), ((Clock, Clock), Value)] = SortedMap.empty,
      values: SortedMap[ID, Value] = SortedMap.empty) {

    def incr: Node = copy(
      value = value + 1,
      values = values + (id -> (valueOf(id) + 1)))

    def join(other: Node): Node =
      if (id == other.id) this
      else Ops.list.foldLeft(this) {
        case (acc, op) => op(acc, other)
      }

    private[HandOff] def valueOf(id: String): Value = values.getOrElse(id, 0)
  }

  // END OF PUBLIC API

  private object Ops {

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

    def mergeVectors(a: Node, b: Node) =
      if (a.tier == 0 && b.tier == 0)
        a.copy(values = Util.mergeWith(Seq(a.values, b.values))(math.max _))
      else a

    def aggregate(a: Node, b: Node) = {

      val below =
        if (a.tier == b.tier) a.below max b.below
        else if (a.tier > b.tier) a.below max b.value
        else a.below

      val value =
        if (a.tier == 0) a.values.map(_._2).sum
        else if (a.tier == b.tier) a.value max b.value max {
          below + a.valueOf(a.id) + b.valueOf(b.id)
        }
        else b.value max (below + a.valueOf(a.id))

      a.copy(
        below = below,
        value = value)
    }

    def discardTokens(a: Node, b: Node) =
      a.copy(
        tokens = a.tokens filterNot {
          case ((src, dst), ((_, dck), _)) =>
            (dst == b.id) && {
              b.slots.get(src) match {
                case Some((_, dck2)) => dck2 > dck
                case None            => b.dck > dck
              }
            }
        })

    def createToken(a: Node, b: Node) =
      b.slots.get(a.id) match {
        case Some((sck, dck)) if sck == a.sck =>
          a.copy(
            tokens = a.tokens + ((a.id -> b.id) -> ((sck -> dck) -> a.valueOf(a.id))),
            values = a.values + (a.id -> 0),
            sck = a.sck + 1)
        case _ => a
      }

    def cacheTokens(a: Node, b: Node) =
      if (a.tier < b.tier) a.copy(
        tokens = Util.mergeWith(
          Seq(
            a.tokens,
            b.tokens filter {
              case ((src, dst), _) => src == b.id && dst != a.id
            })) {
            case (t1@((sck1, _), _), t2@((sck2, _), _)) =>
              if (sck1 >= sck2) t1 else t2
          })
      else a
  }

  private object Util {

    def mergeWith[K: Ordering, V](maps: Seq[SortedMap[K, V]])(f: (V, V) => V): SortedMap[K, V] = {
      maps.foldLeft(SortedMap.empty[K, V]) {
        case (merged, m) =>
          m.foldLeft(merged) {
            case (acc, (k, v)) =>
              acc.get(k).fold(acc + (k -> v)) { existing =>
                acc + (k -> f(existing, v))
              }
          }
      }
    }
  }
}
