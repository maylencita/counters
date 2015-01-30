package crdts

import org.scalatest.{Matchers, FlatSpec}
import scala.collection.SortedMap

class HandOffSpec extends FlatSpec with Matchers {

  import HandOff._

  "A Node" should "join nicely" in {
    val i = Node("i", 2, 
      sck = 2, 
      values = SortedMap("i" -> 9))
    val j = Node("j", 1,
      dck = 5,
      values = SortedMap("j" -> 1021))
    val j2 = j.join(i)
    j2.sck should be (0)
    j2.dck should be (6)
    j2.values should be (SortedMap("j" -> 1021))
    j2.slots should be (SortedMap("i" -> (2, 5)))
  }
}
