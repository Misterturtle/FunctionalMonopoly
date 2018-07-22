package event_bus

import events.Event

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Subscriber {

  type EventCondition = Event => Boolean

  private val _eventConditions: mutable.Map[Class[_ <: Event], Event => Boolean] = mutable.Map[Class[_ <: Event], EventCondition]()

  def checkConditions(event:Event): Boolean = _eventConditions(event.getClass)(event)

  def addConditionForEvent[T <: Event](eventClass:Class[T], condition: T => Boolean) = _eventConditions(eventClass) = condition.asInstanceOf[EventCondition]
}
