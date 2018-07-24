package events

import story.Stateful

trait QueryEvent extends Event

trait TriggerEvent extends Event

case class MutationEvent[A <: Stateful](mutationFn: A => A) extends Event