package io.kagera.dsl.experiment

import fs2.Task
import io.kagera.api.{Marking, PetriNet, ReferenceTokenGame, ScalaGraphPetriNet, TokenGame}
import io.kagera.dsl.experiment.dslExperiment.TransformationArc
import io.kagera.execution.{PetriNetRuntime, TransitionTask, TransitionTaskProvider}
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._
import ops.function._
import syntax.std.function._

import scalax.collection.edge.WLDiEdge
import scalax.collection.immutable.Graph
import shapeless.ops.hlist.HKernelAux

import TransformationArc._

object dslExperiment {

  object TransformationArc {

    private def getTokens(inPlaces: HList, inMarking: Marking[Place]): HList = inPlaces match {
      case HNil ⇒ HNil
      case h :: t => inMarking.apply(h.asInstanceOf[Place[_]]).head._1 :: getTokens(t, inMarking)
    }

    def tokensAt[L <: HList](places: L, markings: Marking[Place]) = getTokens(places, markings)
  }

  case class TransformationArc[F, I <: HList, O <: HList, R <: Product, C <: HList]
    (inputPlaces: I = HList(), transition: Transition[F], outputPlaces: O = HList()) (implicit val fp: FnToProduct.Aux[F, C ⇒ R]) {


    def toPetrinetArc: List[Arc] = ???

    def executeRN[Args <: HList, TL <: HList](args: Args)( implicit genAux: Generic.Aux[R, TL]) = {

      val result = transition.fn.toProduct(args.asInstanceOf[C])
      zipHLs(outputPlaces, genAux.to(result))
    }



    //    def markingTransition: Marking[Place] => Marking[Place] = inMarking ⇒ {
    //
    //      val inAdjTokens = tokensAt(inputPlaces, inMarking)
    //
    //      val outPlacesWithToken = self.executeRN(inAdjTokens)
    //
    //      outPlacesWithToken.runtimeList.foldLeft(inMarking) {
    //        case (m, t) ⇒ t match {
    //          case (p, v) ⇒ m.add(p.asInstanceOf[Place[Any]], v)
    //          case _ ⇒ m
    //        }
    //      }
    //    }


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


    def ~>>[Tup <: Product, TL <: HList, ZL <: HList, Z <: HList, U <: HList](p: Tup)(
      implicit
      retToHList: Generic.Aux[R, ZL],
      productToHList: Generic.Aux[Tup, TL],
      l: LiftAll.Aux[Unwrapped, TL, U],
      comapped: Comapped.Aux[TL, Place, ZL],
      trav2: ToTraversable.Aux[TL, List, Place[_]]) = TransformationArc(inputPlaces, transition, productToHList.to(p))

  }

  implicit class PlaceProductDSL[P <: Product, R<: Product, L <: HList, U <: HList, C <: HList](p: P) {

    def ~>[F](tr: Transition[F])(implicit
                                    g: Generic.Aux[P, L],
                                    fp: FnToProduct.Aux[F, C ⇒ R],
                                    c: Comapped.Aux[L, Place, C],
                                    l: LiftAll.Aux[Unwrapped, L, U],
                                    trav: ToTraversable.Aux[L, List, Place[_]]) =

      TransformationArc(g.to(p), tr)
  }



//  implicit class TransformationArcDSL[F, I <: HList, O <: HList, C <: HList](ptr: TransformationArc[F, I, O])(implicit val c: Comapped.Aux[I, Place, C]) {
//
//
//
//
//    def executeRN[R <: Product, Args <: HList, TL <: HList](args: Args)(
//      implicit
//      fp: FnToProduct.Aux[F, C ⇒ R],
//      genAux: Generic.Aux[R, TL]) = {
//
//      val result = ptr.transition.fn.toProduct(args.asInstanceOf[C])
//      zipHLs(ptr.outputPlaces, genAux.to(result))
//    }
//
//
//    def ~>[R, U <: HList](r: Place[R])(implicit
//                                       l: LiftAll.Aux[Unwrapped, I, U],
//                                       fp: FnToProduct.Aux[F, C ⇒ R],
//                                       trav: ToTraversable.Aux[I, List, Place[_]]) = TransformationArc(ptr.inputPlaces, ptr.transition, outputPlaces = r :: HNil)
//
//    def ~>[R <: Product, Tup <: Product, TL <: HList, ZL <: HList, Z <: HList, U <: HList](p: Tup)(
//      implicit
//      fp: FnToProduct.Aux[F, C ⇒ R],
//      retToHList: Generic.Aux[R, ZL],
//      productToHList: Generic.Aux[Tup, TL],
//      l: LiftAll.Aux[Unwrapped, TL, U],
//      comapped: Comapped.Aux[TL, Place, ZL],
//      trav1: ToTraversable.Aux[I, List, Place[_]],
//      trav2: ToTraversable.Aux[TL, List, Place[_]]) = TransformationArc(ptr.inputPlaces, ptr.transition, productToHList.to(p))
//  }

  def buildPetriNet(a: TransformationArc[_,_,_,_,_]*) = {

    ???

    ///new ScalaGraphPetriNet(Graph(a.flatten: _*))
  }

}

