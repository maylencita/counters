package crdts

import org.scalatest.{Matchers, FlatSpec}

/**
 *
 */
class GCounterSpec extends FlatSpec with Matchers {

  "A GCounter" should "be able to increment its value" in {
    val result = GCounter[String]("A", Map(
      "A" -> 5,
      "B" -> 7)
    ).increment

    result should be (GCounter[String]("A", Map(
      "A" -> 6,
      "B" -> 7)
    ))
    result.get should be (13)
  }

  "A GCounter" should "tell you the global value" in {
    GCounter[String]("A", Map(
      "A" -> 5,
      "B" -> 7)
    ).get should be (12)
  }

  "A GCounter" should "be mergable" in {
        GCounter[String]("A", Map(
          "A" -> 5,
          "B" -> 7)
        )
      .merge(
        GCounter[String]("B", Map(
          "A" -> 4,
          "B" -> 8)
        )
    ) should be (
      GCounter[String]("A",
        Map(
          "A" -> 5,
          "B" -> 8
        ))
    )
  }

}
