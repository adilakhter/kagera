package io.kagera.dsl

import fs2.Task
import io.kagera.api.{Id, Marking, _}
import io.kagera.execution._
import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.{Comapped, LiftAll, ToTraversable}

import scalax.collection.edge.WLDiEdge
import scalax.collection.immutable.Graph
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._
import shapeless.syntax.std.function._
import io.kagera.dsl.experiment.TransformationArc._

package object experiment {

  case class RuntimeTransition[I, O](id: Long, fn: (I, Marking[Place]) => (Marking[Place], O))

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

  case class Place[A](id: String) extends AnyVal {
    def apply[T <: A](_tokens: T*): MarkedPlace[Place, A] = (this, MultiSet(_tokens: _*))
  }

  case class Transition[F](fn: F) extends AnyVal {

    def id = fn.hashCode()
  }

  implicit def transitionId(t: RuntimeTransition[_,_]): Id = Id(t.id)
  implicit def placeId(p: Place[_]): Id = Id(p.hashCode().toLong)

  def |>[A](p: Place[A]): Tuple1[Place[A]] = Tuple1(p)

  def |>[F, R<: Product, C<: HList, I <: HList, O <: HList, ZL <: HList](t: Transition[F])(implicit fp: FnToProduct.Aux[F, C ⇒ R], gen: Generic.Aux[R, ZL]) = TransformationArc(transition = t)

  implicit final class Tuple1ResOps[A](private val self: A) extends AnyVal {
    def -<> = Tuple1(self)
  }

  def hlistToPlaceList(hlist: HList): List[Place[_]] = hlist match {
    case HNil ⇒ List.empty
    case x :: xs ⇒ List(x.asInstanceOf[Place[_]]) ++ hlistToPlaceList(xs)
  }

  import shapeless.ops.hlist.HKernelAux

  def intLen[T <: HList](implicit ker: HKernelAux[T]): Int = ker().length

  def zipHLs(l1: HList, l2: HList): HList = (l1, l2) match {
    case (h1 :: t1, h2 :: t2) => (h1, h2) :: zipHLs(t1, t2)
    case _ => HNil
  }

  // --------

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

    def isColdTransiton: Boolean = inputPlaces.runtimeList.isEmpty

    def executeFn[Args <: HList](args: Args) = {
      transition.fn.toProduct(args.asInstanceOf[C])
    }

    val markingTransition: (Any, Marking[Place]) => (Marking[Place], R) = (input, inMarking) ⇒ {
      val inAdjTokens = tokensAt(inputPlaces, inMarking)
      val output = if (isColdTransiton) input.asInstanceOf[R]  else executeFn(inAdjTokens)
      val outPlacesWithToken = zipHLs(outputPlaces, genAux.to(output))

      val updatedMarkings =
        outPlacesWithToken.runtimeList.foldLeft(Marking.empty[Place]) {
          case (m, t) ⇒ t match {
            case (p, v) ⇒ m.add(p.asInstanceOf[Place[Any]], v)
            case _ ⇒ m
          }
        }

      (updatedMarkings, (1-<>).asInstanceOf[R])
    }

    def toArc: List[Arc] = {
      val runtimeTransition = RuntimeTransition(transition.id, markingTransition)
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

  // --------------


  def buildPetriNet(a: TransformationArc[_,_,_,_,_,_]*) = {

    val arcs: Seq[Arc] = a.toList.flatMap { ta ⇒
      println(s"--converting ${ta} to Arc")
      ta.toArc
    }

    new ScalaGraphPetriNet(Graph(arcs : _*))
  }


  val petriNetRuntime = new PetriNetRuntime[Place, RuntimeTransition, Unit, Unit] {

    val taskProvider: TransitionTaskProvider[Unit, Place, RuntimeTransition] = new TransitionTaskProvider[Unit, Place, RuntimeTransition] {

      def apply[Input, Output](petriNet: PetriNet[Place[_], RuntimeTransition[_, _]], t: RuntimeTransition[Input, Output]): TransitionTask[Place, Input, Output, Unit] = {
        // TODO: compute the output?
        (marking, state, input) ⇒ Task.now(t.fn(input, marking))
      }
    }
  }
}
