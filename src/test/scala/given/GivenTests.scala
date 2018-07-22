package given

import event_bus.EventBus
import events.{Event, TestEvent}
import org.scalatest.{FreeSpec, Matchers}
import story.Story

class GivenTests extends FreeSpec with Matchers {

  "A story should have a given function that takes an event and subscribes to the event bus for that event" in {
    val story = Story(id = 0)

    story.when(classOf[TestEvent])

    EventBus.shared.subscribersFor(classOf[TestEvent]) shouldBe List(story)
  }

  "The given function should also be able to optionally take a conditional function" in {
    val story = Story(id = 0)

    story.when(classOf[TestEvent], condition = { event:TestEvent => event.id == 0})

    val subscriber = EventBus.shared.subscribersFor(classOf[TestEvent]).head

    subscriber.checkConditions(TestEvent(id = 1)) shouldBe false
    subscriber.checkConditions(TestEvent(id = 0)) shouldBe true
  }
}
