package io.kagera.dsl.experiment

import io.kagera.api.Marking
import io.kagera.dsl.experiment.dslExperiment.TransformationArc
import io.kagera.dsl.experiment.dslExperiment.TransformationArc._
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._
import shapeless.syntax.std.function._

object dslExperiment {

  object TransformationArc {

    private def getTokens(inPlaces: HList, inMarking: Marking[Place]): HList = inPlaces match {
      case HNil ⇒ HNil
      case h :: t => inMarking.apply(h.asInstanceOf[Place[_]]).head._1 :: getTokens(t, inMarking)
    }

    def tokensAt[L <: HList](places: L, markings: Marking[Place]): HList = getTokens(places, markings)
  }

  case class TransformationArc[F, I <: HList, O <: HList, R <: Product, C <: HList, ZL <: HList]
    (inputPlaces: I = HList(), transition: Transition[F], outputPlaces: O = HList()) (
      implicit val
        fp: FnToProduct.Aux[F, C ⇒ R],
        genAux: Generic.Aux[R, ZL]) {


    def executeRN[Args <: HList](args: Args) = {
      val result = transition.fn.toProduct(args.asInstanceOf[C])
      zipHLs(outputPlaces, genAux.to(result))
    }

    val markingTransition: Marking[Place] => Marking[Place] = inMarking ⇒ {
      val inAdjTokens = tokensAt(inputPlaces, inMarking)
      val outPlacesWithToken = executeRN(inAdjTokens)

      outPlacesWithToken.runtimeList.foldLeft(inMarking) {
        case (m, t) ⇒ t match {
          case (p, v) ⇒ m.add(p.asInstanceOf[Place[Any]], v)
          case _ ⇒ m
        }
      }
    }

    def toArc: List[Arc] = {
      val runtimeTransition = RuntimeTransition(markingTransition)
      inputPlacesList.map(p => arc(p, runtimeTransition)) ++ outputPlacesList.map(p => arc(runtimeTransition, p))
    }

    def inputPlacesList = hlistToPlaceList(inputPlaces)

    def outputPlacesList = hlistToPlaceList(outputPlaces)


    def ~>>[Tup <: Product, TL <: HList, Z <: HList, U <: HList](p: Tup)(
      implicit
      productToHList: Generic.Aux[Tup, TL],
      l: LiftAll.Aux[Unwrapped, TL, U],
      comapped: Comapped.Aux[TL, Place, ZL],
      trav2: ToTraversable.Aux[TL, List, Place[_]]) = TransformationArc(inputPlaces, transition, productToHList.to(p))
  }

  implicit class PlaceProductDSL[P <: Product, R <: Product, L <: HList, U <: HList, C <: HList](p: P) {

    def ~>[F, ZL <: HList](tr: Transition[F])(implicit
                                              g: Generic.Aux[P, L],
                                              fp: FnToProduct.Aux[F, C ⇒ R],
                                              rg: Generic.Aux[R, ZL],
                                              c: Comapped.Aux[L, Place, C],
                                              l: LiftAll.Aux[Unwrapped, L, U],
                                              trav: ToTraversable.Aux[L, List, Place[_]]) =

      TransformationArc(g.to(p), tr)
  }
}

