package io.kagera.dsl.experiment

import io.kagera.dsl.experiment.dslExperiment.TransformationArc
import shapeless.HList

object runtime {

  //  val petriNetRuntime = new PetriNetRuntime[Place, RuntimeTransition, Unit, Unit] {
  //    override val tokenGame: TokenGame[Place[_], RuntimeTransition[_, _], Marking[Place]] = new ReferenceTokenGame[Place, RuntimeTransition]
  //    override val taskProvider: TransitionTaskProvider[Unit, Place, RuntimeTransition] = new TransitionTaskProvider[Unit, Place, RuntimeTransition] {
  //
  //      override def apply[Input, Output](petriNet: PetriNet[Place[_], RuntimeTransition[_, _]], t: RuntimeTransition[Input, Output]): TransitionTask[Place, Input, Output, Unit] = {
  //        (marking, state, input) => ???
  //          //Task.now(t.fn(marking))
  //      }
  //    }
  //  }

}
