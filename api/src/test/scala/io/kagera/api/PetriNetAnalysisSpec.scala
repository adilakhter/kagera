package io.kagera.api

import io.kagera.dsl.simple._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class PetriNetAnalysisSpec extends WordSpec {

  "The PetriNetAnalysis object" should {

    "correctly asses the reachability of a very simple petri net" in {

      val pn = createPetriNet(
        1 ~|~> (2, 3),
        2 ~|~> 4,
        3 ~|~> 5,
        (4, 5) ~|~> 6
      )

      val initialMarking = Map(1 -> 1)
      val targetMarking = Map(6 -> 1)

      val analysis = new PetriNetAnalysis(pn)

      analysis.isCoverable(initialMarking, targetMarking) shouldBe true
    }
  }
}
