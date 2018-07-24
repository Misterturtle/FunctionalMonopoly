package given

import event_bus.{AddressBook, EventBus, Address}
import events.TriggerEvent
import org.scalatest.{FreeSpec, Matchers}
import story.{State, Stateful, Story}
import scala.reflect.runtime.universe._

class EventBusTests extends FreeSpec with Matchers {

  "playground" in {

    case class Entity1(address:Address, value: String) extends Stateful {

      val story = Story[Entity1]
        .whenTriggerEvent(typeTag[TestTriggerEvent])
        .thenBroadcastMutation(self => self.copy(value = "Changed"))

      val stories = List(story)
      val newState = State(stories)

      override protected val states: List[State] = List(newState)
    }

    val eventBus = new EventBus()
    val addressBook = new AddressBook()
    EventBus.shared = eventBus

    val entity = Entity1(Address("/root"), "Not Change")
    eventBus.post(TestTriggerEvent(), typeTag[TestTriggerEvent])

    val newEntity = addressBook.getStateful(Address("/root")).asInstanceOf[Entity1]

    newEntity.value shouldBe "Changed"
  }


}

case class TestTriggerEvent() extends TriggerEvent
