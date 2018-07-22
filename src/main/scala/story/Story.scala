package story

import event_bus.{EventBus, Subscriber}
import events.{Event, EventType, QueryEvent, TestEvent}

case class Story(id:Int) extends Subscriber {
  val eventBus = EventBus.shared

  def when[T <: Event](eventClass:Class[T], condition: T => Boolean = { _:T => true}) : Story = {
    addConditionForEvent(eventClass, condition)
    eventBus.subscribeTo(eventClass, this)
    this
  }

  def given(queryConditional: QueryEvent => Boolean): Story = {
    this
  }

}
