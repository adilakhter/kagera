package io.kagera.dsl.experiment

import dslExperiment._
import io.kagera.api.Marking
import runtime._
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

  //  buildPetriNet(
  //    (|>(p1) ~> tr1).toPetrinetArc,
  //    ((p1, p2, p3) ~> tr3 ~> p5).toPetrinetArc)

  //  Working examples:

  //
  //  |>  (tr0) ~>> p1
  //
  //  |>  (tr02) ~>> ((p1, p2))

  //  val netDef2 = pnet(
  |>(tr0) ~>> |>(p1)
  |>(tr02) ~>> ((p2, p3))
  (p1, p2, p3) ~> tr3 ~>> |>(p5)
  //println(((p1, p2, p3)  ~>  tr32) ~>> ((p4, p1)))
  //      (p1, p2)      ~>  tr2,
  //      |> (p1)       ~>  tr1
  //  )

  // Given a Marking
  val m = Marking(p1(1), p4("foo"))

  val tr33 = Transition((x: Int, y: Int, z: Int) ⇒ Tuple1(x + y + z + ""))
  val tr11 = Transition((x: Int) ⇒ Tuple1(x + 1000))

  val dsl = (p1, p2, p3) ~> tr33 ~>> |>(p5)
  val dsl2 = TransformationArc(p1 :: HNil, tr11, p2 :: HNil)

  (p1, p2, p3) ~> tr33 ~>> |>(p5)

  |>(p1) ~> tr11 ~>> |>(p2)

  import TransformationArc._
  val tokens11 = tokensAt(dsl2.inputPlaces, m)

  val result11 = dsl2.executeRN(tokens11).runtimeList.foldLeft(m) {
    case (m, t) ⇒ t match {
      case (p, v) ⇒ m.add(p.asInstanceOf[Place[Any]], v)
      case _      ⇒ m
    }
  }

  println(m)
  println(result11)

  println(result11.get(p2))
}
