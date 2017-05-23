package io.kagera.dsl.colored.transitions

import io.kagera.dsl.colored.Transition
import io.kagera.execution.ExceptionStrategy.BlockTransition
import io.kagera.execution.TransitionExceptionHandler

abstract class AbstractTransition[I, O, S](
    override val id: Long,
    override val label: String,
    override val isAutomated: Boolean,
    override val exceptionStrategy: TransitionExceptionHandler = (e, n) ⇒ BlockTransition) extends Transition[I, O, S] {

}

