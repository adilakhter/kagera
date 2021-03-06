package io.kagera.akka

import io.kagera.dsl.colored.SequenceNet

sealed trait Event
case class Added(n: Int) extends Event
case class Removed(n: Int) extends Event

trait TestSequenceNet extends SequenceNet[Set[Int], Event] {

  override val eventSourceFunction: Set[Int] ⇒ Event ⇒ Set[Int] = set ⇒ {
    case Added(c)   ⇒ set + c
    case Removed(c) ⇒ set - c
  }
}
