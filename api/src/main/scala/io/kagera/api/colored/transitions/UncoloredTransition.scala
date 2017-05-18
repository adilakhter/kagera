package io.kagera.api.colored.transitions

import fs2.Task
import io.kagera.api.colored._
import io.kagera.api.multiset.{ MultiSet, _ }

import scala.concurrent.{ ExecutionContext, Future }

trait UncoloredTransition[Input, Output, State] extends Transition[Input, Output, State] {

  override def apply(inAdjacent: MultiSet[Place[_]], outAdjacent: MultiSet[Place[_]]) = {
    (consume, state, input) ⇒
      {
        // assumes uncolored outgoing places (Place[Unit])
        val produce = outAdjacent.map { case (p, count) ⇒ p -> Map(() -> count) }.toMarking
        produceEvent(consume, state, input).map(output ⇒ (produce, output))
      }
  }

  def produceEvent(consume: Marking[Place], state: State, input: Input): Task[Output]
}
