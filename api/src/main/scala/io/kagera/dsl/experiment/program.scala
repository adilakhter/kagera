package io.kagera.dsl.experiment

import io.kagera.api.Marking
import shapeless._

object program extends App {

  import dslExperiment._

  val p1 = Place[Int](id = "t1")
  val p2 = Place[Int](id = "2")
  val p3 = Place[Int](id = "3")

  val p4 = Place[String](id = "string1")
  val p5 = Place[String](id = "string2")

  val tr0 = Transition(() ⇒ Tuple1(1))
  val tr02 = Transition(() ⇒ (1, 2))
  val tr1 = Transition((x: Int) ⇒ x + 1)
  val tr12 = Transition((x: Int) ⇒ (1, 2))
  val tr2 = Transition((x: Int, y: Int) ⇒ x + 1)
  val tr3 = Transition((x: Int, y: Int, z: Int) ⇒ Tuple1(x + y + z + ""))
  val tr32 = Transition((x: Int, y: Int, z: Int) ⇒ (x + y + z + "", 10))

  // Given a Marking
  val m = Marking(p1(1), p4("foo"))

  // And two Transition Functions
  val tr33 = Transition((x: Int, y: Int, z: Int) ⇒ Tuple1(x + y + z + ""))
  val tr11 = Transition((x: Int) ⇒ Tuple1(x + 1000))

  // We can define a TransformationArcs
  val transitionArcExample = TransformationArc(p1 :: HNil, tr11, p2 :: HNil)

  // Apply the markingTransition Function.
  // It will apply the Transition fn and update the Markings
  assert(transitionArcExample.markingTransition(m).get(p2).isDefined)

  // Builder that allows building a Net structure and returns Seq of Arc
  val resultingNet =
    buildPetriNet(
      (p1, p2, p3) ~> tr33 ~>> |>(p5),
      |>(p1) ~> tr11 ~>> |>(p2)
    )

  import runtime._
  // TODO: Run the PetriNet

}
