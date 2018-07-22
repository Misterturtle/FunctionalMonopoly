package given

import akka.actor.ActorPath
import event_bus.EventBus
import events.{QueryEvent, TestEvent, TestEventStatus}
import org.scalatest.{FreeSpec, Matchers}
import story.Story

class WhenTests extends FreeSpec with Matchers {


  "A story should have a when method that takes an actor path lookup and a condition" in {
    val story = Story[StoryClass](id = 0)
        .givenQueryEvent(TestEventStatus(10, "None"))
        .respondsWith{ case testEventResponse: TestEventStatus => testEventResponse.status == "Hello World"}
        .whenTriggerEvent(TestEvent.getClass)
        .meetCondition{ case testEvent: TestEvent => testEvent.id == 0}
        .thenBroadcastEvent{ case storyClass: StoryClass => storyClass.copy(value = "I've been modified")}
        .thenFireEvent(StoryClassChangedEvent())

  }


}


case class StoryClass(value: String)

case class StoryClassChangedEvent()