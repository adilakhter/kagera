package io.kagera.dsl.experiment

import fs2.Task
import io.kagera.api.{Marking, PetriNet, ReferenceTokenGame, ScalaGraphPetriNet, TokenGame}
import io.kagera.execution.{PetriNetRuntime, TransitionTask, TransitionTaskProvider}
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._
import shapeless.poly._


import scalax.collection.edge.WLDiEdge
import scalax.collection.immutable.Graph


object dslExperiment extends App {


  case class Place[A](id: String) extends AnyVal

  case class Transition[F](fn: F) extends AnyVal

  case class TransformationArc[F, I <: HList, O <: HList](inputPlaces: I = HList(), transition: Transition[F], outputPlaces: O = HList()){

    object valueForPlace extends Poly1 {
      implicit def default[T] = at[Place[T]] (p => p)
    }


    def markingFn[R <: HList](consume: Marking[Place])(implicit mapper: Mapper.Aux[valueForPlace.type , I, R]):  Marking[Place] = {
      //def valueForPlace[T](p: Place[T]): T = consume.apply[T](p).head._1
      inputPlaces.map(valueForPlace)
      consume
    }

    def toPetrinetArc[R <: HList](implicit  t1: ToTraversable.Aux[I, List, Place[_]], t2: ToTraversable.Aux[O, List, Place[_]], mapper: Mapper.Aux[valueForPlace.type , I, R]): List[Arc] = {
      val runtimeTransition = RuntimeTransition(markingFn)

      val markingFn: Marking[Place] => Marking[Place] = consume => {

        // TODO
        // 1. Convert consume (input marking: Place[T] -> MultiSet[T]) to input tuple (A, B, C) for function (A, B, C) => (B, D)
        // 2. Call the function with the input tuple resulting in (B, D)
        // 3. Convert output tuple (B, D) into output marking (Place[T] -> MultiSet[T]), return it

        val foo = inputPlacesList.map(p => consume.apply(p).head._1)
      }

      


      inputPlacesList.map(p => arc(p, runtimeTransition)) ++ outputPlacesList.map(p => arc(runtimeTransition, p))
    }

    def inputPlacesList(implicit  trav: ToTraversable.Aux[I, List, Place[_]]): List[Place[_]] = inputPlaces.toList

    def outputPlacesList(implicit  trav: ToTraversable.Aux[O, List, Place[_]]): List[Place[_]] = outputPlaces.toList

    // Int, Int  =>  Int
    // Any => Any
    // TODO: use a generic FN
    def fn: Any => Any = transition.fn.asInstanceOf[Any => Any]

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

  case class RuntimeTransition[I, O](fn: Marking[Place] => Marking[Place])

  /**
    * Type alias for the node type of the scalax.collection.Graph backing the petri net.
    */
  type Node = Either[Place[_], RuntimeTransition[_, _]]

  /**
    * Type alias for the edge type of the scalax.collection.Graph backing the petri net.
    */
  type Arc = WLDiEdge[Node]

  def arc(t: RuntimeTransition[_, _], p: Place[_]): Arc = WLDiEdge[Node, String](Right(t), Left(p))(1, "")

  def arc[C](p: Place[C], t: RuntimeTransition[_, _]): Arc = WLDiEdge[Node, String](Left(p), Right(t))(1, "")

  def buildPetriNet(a: List[Arc]*) = {


    new ScalaGraphPetriNet(Graph(a.flatten: _*))
  }

  val petriNetRuntime = new PetriNetRuntime[Place, RuntimeTransition, Unit, Unit] {
    override val tokenGame: TokenGame[Place[_], RuntimeTransition[_, _], Marking[Place]] = new ReferenceTokenGame[Place, RuntimeTransition]
    override val taskProvider: TransitionTaskProvider[Unit, Place, RuntimeTransition] = new TransitionTaskProvider[Unit, Place, RuntimeTransition] {

      override def apply[Input, Output](petriNet: PetriNet[Place[_], RuntimeTransition[_, _]], t: RuntimeTransition[Input, Output]): TransitionTask[Place, Input, Output, Unit] = {
        (marking, state, input) => ???
          //Task.now(t.fn(marking))
      }
    }
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

  buildPetriNet(
  (|>  (p1)  ~> tr1).toPetrinetArc,
  ( (p1, p2, p3)  ~>  tr3 ~> p5).toPetrinetArc)


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
}

