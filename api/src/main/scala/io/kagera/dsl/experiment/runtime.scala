package io.kagera.dsl.experiment

import fs2.Task
import io.kagera.api.{ Marking, PetriNet, ReferenceTokenGame, TokenGame }
import io.kagera.dsl.experiment.dslExperiment.TransformationArc
import io.kagera.execution.{ PetriNetRuntime, TransitionTask, TransitionTaskProvider }
import shapeless.HList

object runtime {

  val petriNetRuntime = new PetriNetRuntime[Place, RuntimeTransition, Unit, Unit] {

    val tokenGame: TokenGame[Place[_], RuntimeTransition[_, _], Marking[Place]] = new ReferenceTokenGame[Place, RuntimeTransition]

    val taskProvider: TransitionTaskProvider[Unit, Place, RuntimeTransition] = new TransitionTaskProvider[Unit, Place, RuntimeTransition] {

      def apply[Input, Output](petriNet: PetriNet[Place[_], RuntimeTransition[_, _]], t: RuntimeTransition[Input, Output]): TransitionTask[Place, Input, Output, Unit] = {
        // TODO: compute the output?
        (marking, state, input) ⇒ Task.now(t.fn(marking))
      }
    }
  }
}
