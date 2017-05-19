package io.kagera.execution

import cats.data.State
import io.kagera.api.TokenGame
import io.kagera.api._

/**
 * Given a token game picks the next job(s) to be executed.
 *
 * TODO: this should be a trait
 */
class JobPicker[P[_], T[_, _, _]](tokenGame: TokenGame[P[_], T[_, _, _], Marking[P]]) {

  /**
   * Fires a specific transition with input, computes the marking it should consume
   */
  def createJob[S, I, E](transition: T[I, E, S], input: I): State[Instance[P, T, S], Either[String, Job[P, T, S, E]]] =
    State { instance ⇒
      instance.isBlockedReason(transition) match {
        case Some(reason) ⇒
          (instance, Left(reason))
        case None ⇒
          tokenGame.enabledParameters(instance.process)(instance.availableMarking).get(transition) match {
            case None ⇒
              (instance, Left(s"Not enough consumable tokens"))
            case Some(params) ⇒
              val (updatedState, job) = createJob(transition.asInstanceOf[T[Any, E, S]], params.head, input)(instance)
              (updatedState, Right(job))
          }
      }
    }

  /**
   * Creates a job for a specific input & marking. Does not do any validation on the parameters
   */
  private def createJob[E, S](transition: T[Any, E, S], consume: Marking[P], input: Any): Instance[P, T, S] ⇒ (Instance[P, T, S], Job[P, T, S, E]) = instance ⇒ {
    val job = Job[P, T, S, E](instance.nextJobId(), instance.state, transition, consume, input)
    val updatedInstance = instance.copy[P, T, S](jobs = instance.jobs + (job.id -> job))
    (updatedInstance, job)
  }

  def isFireable[S](instance: Instance[P, T, S], t: T[_, _, _]) = {
    !(instance.isBlockedReason(t).isDefined || instance.process.incomingPlaces(t).isEmpty)
  }

  /**
   * Finds the (optional) first transition that is automated & enabled
   */
  def firstNewEnabledJob[S]: State[Instance[P, T, S], Option[Job[P, T, S, _]]] = State { instance ⇒
    tokenGame.enabledParameters(instance.process)(instance.availableMarking).find {
      case (t, markings) ⇒ isFireable(instance, t)
    }.map {
      case (t, markings) ⇒
        val job = Job[P, T, S, Any](instance.nextJobId(), instance.state, t.asInstanceOf[T[Any, Any, S]], markings.head, ())
        (instance.copy[P, T, S](jobs = instance.jobs + (job.id -> job)), Some(job))
    }.getOrElse((instance, None))
  }

  /**
   * Finds all automated enabled transitions.
   */
  def enabledJobs[S]: State[Instance[P, T, S], Set[Job[P, T, S, _]]] =
    firstNewEnabledJob[S].flatMap {
      case None      ⇒ State.pure(Set.empty)
      case Some(job) ⇒ enabledJobs[S].map(_ + job)
    }
}
