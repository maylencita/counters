package crdts

/**
 *
 * 
 * @tparam IdT type of the node identifier
 */
class GCounter[IdT](private val id: IdT, private val vals: Map[IdT, Long]) {

  // -------------------------------------------- Public methods

  /** Increments the counter locally with the given delta. */
  def increment(delta: Long = 1): GCounter[IdT] =
    new GCounter(id, {
      val local = vals.getOrElse[Long](id, 0)
      vals + (id -> (local + delta))
    })

  /** Gets the value of the counter */
  def get: Long = vals.values.sum

  /** Merges this G-counter instance with another G-counter to apply any updates from it. */
  def merge(other: GCounter[IdT]): GCounter[IdT] =
    new GCounter(id, {
      val allNodes = vals.keySet ++ other.vals.keySet
      allNodes.map({ node =>
        val max = Math.max(
                vals.getOrElse[Long](node, 0),
          other.vals.getOrElse[Long](node, 0)
        )
        node -> max
      }).toMap
    })

}
