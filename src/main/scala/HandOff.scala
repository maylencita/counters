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
      tokens: SortedMap[ID, (Clock, (Clock, Clock))],
      values: SortedMap[ID, Value]) {

    def incr: Node = copy(
      value = value + 1,
      values = values.get(id).fold(SortedMap(id -> 1)) { v =>
        values + (id -> (v + 1))
      })

    def join(other: Node): Node =
      if (id == other.id) this
      else Ops.list.foldLeft(this) {
        case (acc, op) => op(acc, other)
      }
  }

  object Ops {

    val list: List[(Node, Node) => Node] = List(
      fillSlots)

    def fillSlots(a: Node, b: Node) = a

    def discardSlot(a: Node, b: Node) = a

    def mergeVectors(a: Node, b: Node) = a

    def aggregate(a: Node, b: Node) = a

    def discardTokens(a: Node, b: Node) = a

    def createTokens(a: Node, b: Node) = a

    def cacheTokens(a: Node, b: Node) = a
  }
}
