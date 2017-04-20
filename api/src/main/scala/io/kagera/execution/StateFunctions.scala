package io.kagera.execution

import cats.data.State
import fs2.Task
import io.kagera.api._
import io.kagera.api.colored.{ Transition, _ }
import io.kagera.execution.EventSourcing.TransitionEvent

object StateFunctions {

  /**
   * Fires a specific transition with input, computes the marking it should consume
   */
  def createJob[S, E](transition: Transition[Any, E, S], input: Any): State[Instance[S], Either[String, Job[S, E]]] =
    State { instance ⇒
      instance.isBlockedReason(transition.id) match {
        case Some(reason) ⇒
          (instance, Left(reason))
        case None ⇒
          instance.process.enabledParameters(instance.availableMarking).get(transition) match {
            case None ⇒
              (instance, Left(s"Not enough consumable tokens"))
            case Some(params) ⇒
              val (updatedState, job) = createJob(transition, params.head, input)(instance)
              (updatedState, Right(job))
          }
      }
    }

  /**
   * Creates a job for a specific input & marking. Does not do any validation on the parameters
   */
  def createJob[E, S](transition: Transition[Any, E, S], consume: Marking, input: Any): Instance[S] ⇒ (Instance[S], Job[S, E]) = s ⇒ {
    val job = Job[S, E](s.nextJobId(), s.state, transition.id, consume, input)
    val newState = s.copy(jobs = s.jobs + (job.id -> job))
    (newState, job)
  }

  /**
   * Creates a Job for a transition.
   */
  def createJob[S](transitionId: Long, input: Any): State[Instance[S], Either[String, Job[S, Any]]] =
    State.inspect[Instance[S], Option[Transition[Any, Any, S]]] { instance ⇒
      instance.process.transitions.findById(transitionId).map(_.asInstanceOf[Transition[Any, Any, S]])
    }.flatMap {
      case None    ⇒ State.pure(Left(s"No transition exists with id $transitionId"))
      case Some(t) ⇒ createJob(t, input)
    }

  /**
   * Finds the (optional) first transition that is automated & enabled
   */
  def firstNewEnabledJob[S]: State[Instance[S], Option[Job[S, _]]] = State { instance ⇒
    instance.process.enabledParameters(instance.availableMarking).find {
      case (t, markings) ⇒ t.isAutomated && !instance.isBlockedReason(t.id).isDefined
    }.map {
      case (t, markings) ⇒
        val job = Job[S, Any](instance.nextJobId(), instance.state, t.id, markings.head, ())
        (instance.copy(jobs = instance.jobs + (job.id -> job)), Some(job))
    }.getOrElse((instance, None))
  }

  /**
   * Finds all automated enabled transitions.
   */
  def newEnabledJobs[S]: State[Instance[S], Set[Job[S, _]]] =
    firstNewEnabledJob[S].flatMap {
      case None      ⇒ State.pure(Set.empty)
      case Some(job) ⇒ newEnabledJobs[S].map(_ + job)
    }

  def applyJobs[S, P[_], T[_, _, _]](executor: TransitionExecutor[S, P, T])(jobs: Set[Job[S, _]]): State[Instance[S], List[TransitionEvent]] = {
    State { instance ⇒
      val events = Task.traverse(jobs.toSeq)(job ⇒ executor.runJobAsync(job)).unsafeRun()
      val updated = events.foldLeft(instance) { (s, e) ⇒
        val appliedEvent = EventSourcing.apply(s)(e)
        appliedEvent.copy(jobs = appliedEvent.jobs - e.jobId)
      }
      (updated, events.toList)
    }
  }
}
