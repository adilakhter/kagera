package io.kagera.dsl.experiment

import fs2.Task
import io.kagera.api.{Marking, PetriNet, ReferenceTokenGame, ScalaGraphPetriNet, TokenGame}
import io.kagera.execution.{PetriNetRuntime, TransitionTask, TransitionTaskProvider}
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._
import ops.function._
import syntax.std.function._

import scalax.collection.edge.WLDiEdge
import scalax.collection.immutable.Graph


object dslExperiment {

  object TransformationArc {
    def getTokens(inPlaces: HList, inMarking: Marking[Place]): HList = inPlaces match {
      case HNil ⇒ HNil
      case h :: t => inMarking.apply(h.asInstanceOf[Place[_]]).head._1 :: getTokens(t, inMarking)
    }
  }

  case class TransformationArc[F, I <: HList, O <: HList](inputPlaces: I = HList(), transition: Transition[F], outputPlaces: O = HList()){
    import TransformationArc._

    def tokens(inMarking: Marking[Place]) = getTokens(inputPlaces, inMarking)

    def toPetrinetArc(implicit  t1: ToTraversable.Aux[I, List, Place[_]], t2: ToTraversable.Aux[O, List, Place[_]]): List[Arc] = ???

//    def toPetrinetArc(implicit  t1: ToTraversable.Aux[I, List, Place[_]], t2: ToTraversable.Aux[O, List, Place[_]]): List[Arc] = {
//
//      val markingFn: Marking[Place] => Marking[Place] = consume => {
//
//        // TODO
//        // 1. Convert consume (input marking: Place[T] -> MultiSet[T]) to input tuple (A, B, C) for function (A, B, C) => (B, D)
//        // 2. Call the function with the input tuple resulting in (B, D)
//        // 3. Convert output tuple (B, D) into output marking (Place[T] -> MultiSet[T]), return it
//
//        val inTokens = tokens(inTokens, consume)
//
//        val fn = transition.fn.toProduct
//
//        val foo = inputPlacesList.map(p => consume.apply(p).head._1)
//      }
//
//      val runtimeTransition = RuntimeTransition(markingFn)
//      inputPlacesList.map(p => arc(p, runtimeTransition)) ++ outputPlacesList.map(p => arc(runtimeTransition, p))
//    }

    def inputPlacesList = hlistToPlaceList(inputPlaces)

    def outputPlacesList = hlistToPlaceList(outputPlaces)

    // Int, Int  =>  Int
    // Any => Any
    // TODO: use a generic FN
    def fn: Any => Any = transition.fn.asInstanceOf[Any => Any]


    // Part of DSL
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

    def executeFn[R, Args <: HList](args: Args) (implicit fp: FnToProduct.Aux[F, C ⇒ R]) =
      ptr.transition.fn.toProduct(args.asInstanceOf[C])

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

}

