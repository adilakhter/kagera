package io.kagera.api

import io.kagera.dsl.simple._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class PetriNetAnalysisSpec extends WordSpec {

  "The PetriNetAnalysis class" should {

    "correctly asses the reachability of a very simple petri net A" in {

      val pn = createPetriNet(
        1 ~|~> (2, 3),
        2 ~|~> 4,
        3 ~|~> 5,
        (4, 5) ~|~> 6,
        1 ~|~> 7
      )

      val initialMarking = Map(1 -> 1)
      val targetMarking = Map(6 -> 1)

      val analysis = new PetriNetAnalysis(pn)

      analysis.isCoverable(initialMarking, Map(2 -> 1, 3 -> 1, 4 -> 1)) shouldBe false
      analysis.isCoverable(initialMarking, Map(4 -> 1)) shouldBe true
      analysis.isCoverable(initialMarking, Map(5 -> 1)) shouldBe true
      analysis.isCoverable(initialMarking, Map(6 -> 1)) shouldBe true
      analysis.isCoverable(initialMarking, Map(6 -> 1, 1 -> 1)) shouldBe false

      analysis.isReachable(initialMarking, Map(4 -> 1)) shouldBe false
      analysis.isReachable(initialMarking, Map(7 -> 1)) shouldBe true
      analysis.isReachable(initialMarking, Map(6 -> 1, 7 -> 1)) shouldBe false
    }
  }
}
