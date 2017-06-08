package io.kagera.dsl.experiment

import dslExperiment._
import io.kagera.api.Marking
import runtime._
import shapeless._

object program extends App {

  val p1 = Place[Int](id = "t1")
  val p2 = Place[Int](id = "2")
  val p3 = Place[Int](id = "3")

  val p4 = Place[String](id = "string1")
  val p5 = Place[String](id = "string2")

  val tr0 = Transition(() ⇒ 1)
  val tr02 = Transition(() ⇒ (1, 2))
  val tr1 = Transition((x: Int) ⇒ x + 1)
  val tr2 = Transition((x: Int, y: Int) ⇒ x + 1)
  val tr3 = Transition((x: Int, y: Int, z: Int) ⇒ x + y + z + "")
  val tr32 = Transition((x: Int, y: Int, z: Int) ⇒ (x + y + z + "", 10))

  //  buildPetriNet(
  //    (|>(p1) ~> tr1).toPetrinetArc,
  //    ((p1, p2, p3) ~> tr3 ~> p5).toPetrinetArc)

  //  Working examples:

  //
  //  |>  (tr0) ~>> p1
  //
  //  |>  (tr02) ~>> ((p1, p2))

  //  val netDef2 = pnet(
  //      |> (tr0)     ~>>  p1,
  //      |> (tr02)    ~>>  ((p2, p3)),
  //      (p1, p2, p3)  ~>  tr3 ~> p5,
  //      (p1, p2, p3)  ~>  tr32 ~> ((p4, p1)),
  //      (p1, p2)      ~>  tr2,
  //      |> (p1)       ~>  tr1
  //  )

  // Given a Marking
  val m = Marking(p1(1), p4("foo"))
  // And a Transformation Arc
  val a = TransformationArc(p1 :: HNil, tr1, p2 :: HNil)

  // We can get the all the places
  val inPlaces = a.inputPlacesList
  val outPlaces = a.outputPlacesList

  // And the tokens from In Places
  val tokensFromInPlaces = a.tokens(m)

  // And Executing with the tokens from a Marking
  val result = a.executeFn(tokensFromInPlaces)

  // And map the result in output places
  // TODO. 
}
