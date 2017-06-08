package io.kagera.dsl.colored

import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._


object dslExperiment extends App {

  case class Place[A](id: String) extends AnyVal

  case class Transition[F](fn: F) extends AnyVal

  case class TransformationArc[F, I <: HList, O <: HList](inputPlaces: I = HList(), transition: Transition[F], outputPlaces: O = HList()){

    def ~>>[R, U <: HList, C <: HList](r: Place[R])(implicit fp: FnToProduct.Aux[F, C ⇒ R]) = TransformationArc(inputPlaces, transition, outputPlaces = r :: HNil)


    def ~>>[R <: Product, C <: HList, Tup <: Product, TL <: HList, ZL <: HList, Z <: HList, U <: HList](p: Tup)(
      implicit
      fp: FnToProduct.Aux[F, C ⇒ R],
      retToHList: Generic.Aux[R, ZL],
      productToHList: Generic.Aux[Tup, TL],
      l: LiftAll.Aux[Unwrapped, TL, U],
      comapped: Comapped.Aux[TL, Place, ZL],
      trav2: ToTraversable.Aux[TL, List, Place[_]]) = TransformationArc(inputPlaces, transition, productToHList.to(p))

  }

  implicit class PlaceProductDSL[P <: Product, L <: HList, U <: HList, C <: HList](p: P){

    def ~>[F, R](tr: Transition[F])(implicit
                                    fp: FnToProduct.Aux[F, C ⇒ R],
                                    g: Generic.Aux[P, L],
                                    c: Comapped.Aux[L, Place, C],
                                    l: LiftAll.Aux[Unwrapped, L, U],
                                    trav: ToTraversable.Aux[L, List, Place[_]]) =

      TransformationArc(g.to(p), tr)

  }

  implicit class TransformationArcDSL[F, I <: HList, O <: HList, C <: HList](ptr: TransformationArc[F, I, O])(implicit val c: Comapped.Aux[I, Place, C]) {

    def ~>[R, U <: HList](r: Place[R])(implicit
                                       l: LiftAll.Aux[Unwrapped, I, U],
                                       fp: FnToProduct.Aux[F, C ⇒ R],
                                       trav: ToTraversable.Aux[I, List, Place[_]]) = TransformationArc(ptr.inputPlaces, ptr.transition,  outputPlaces = r :: HNil)

    def ~>[R <: Product, Tup <: Product, TL <: HList, ZL <: HList, Z <: HList, U <: HList](p: Tup)(
      implicit
      fp: FnToProduct.Aux[F, C ⇒ R],
      retToHList: Generic.Aux[R, ZL],
      productToHList: Generic.Aux[Tup, TL],
      l: LiftAll.Aux[Unwrapped, TL, U],
      comapped: Comapped.Aux[TL, Place, ZL],
      trav1: ToTraversable.Aux[I, List, Place[_]],
      trav2: ToTraversable.Aux[TL, List, Place[_]]) = TransformationArc(ptr.inputPlaces, ptr.transition, productToHList.to(p))
  }

  def pnet(args: TransformationArc[_, _, _]*) = {
    println(args.mkString("\n"))

    // TODO: build PN
  }

  val p1 = Place[Int](id = "t1")
  val p2 = Place[Int](id = "2")
  val p3 = Place[Int](id = "3")

  val p4 = Place[String](id = "string1")
  val p5 = Place[String](id = "string2")

  val tr0 = Transition(() ⇒ 1)
  val tr02 = Transition(() ⇒ (1,2))
  val tr1 = Transition((x: Int) ⇒ x + 1)
  val tr2 = Transition((x: Int, y: Int) ⇒ x + 1)
  val tr3 = Transition((x: Int, y: Int, z: Int) ⇒ x + y + z + "")
  val tr32 = Transition((x: Int, y: Int, z: Int) ⇒ (x + y + z + "", 10))

  def |>[A](p: Place[A]): Tuple1[Place[A]] = Tuple1(p)


  def |>[F, I <: HList, O <: HList](t: Transition[F]) = TransformationArc(transition = t)

//  Working examples:
//  |>  (p1)  ~> tr1
//
//  |>  (tr0) ~>> p1
//
//  |>  (tr02) ~>> ((p1, p2))


  val netDef2 = pnet(
      |> (tr0)     ~>>  p1,
      |> (tr02)    ~>>  ((p2, p3)),
      (p1, p2, p3)  ~>  tr3 ~> p5,
      (p1, p2, p3)  ~>  tr32 ~> ((p4, p1)),
      (p1, p2)      ~>  tr2,
      |> (p1)       ~>  tr1
  )
}

