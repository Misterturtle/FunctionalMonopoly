package events

import story.Stateful
import scala.reflect.runtime.universe.{TypeTag, typeTag}

trait QueryEvent extends Event

trait TriggerEvent extends Event

case class MutationEvent[A : TypeTag](mutationFn: A => A)(implicit val tt: TypeTag[MutationEvent[A]]) extends Event