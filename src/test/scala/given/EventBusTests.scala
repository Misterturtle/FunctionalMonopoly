package given

import event_bus.{Address, AddressBook, EventBus}
import events.TriggerEvent
import given.Ent1.testStory
import org.scalatest.{FreeSpec, Matchers}
import story.{State, Stateful, Story, StoryVerbage}

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

class EventBusTests extends FreeSpec with Matchers {






  "playground" in {

    val eventBus = EventBus.shared
    val addressBook = AddressBook.shared

    val entity = Entity1(Address("/root"), "Not Change")

    eventBus.post(TestTriggerEvent(), typeTag[TestTriggerEvent])

    val newEntity = addressBook.getStateful(Address("/root"))._1.asInstanceOf[Entity1]

    newEntity.value shouldBe "Changed"
  }


}

case class TestTriggerEvent() extends TriggerEvent

case object Ent1 {

  val testStory: StoryVerbage[Entity1] = Story[Entity1]
    .whenTriggerEvent(typeTag[TestTriggerEvent])
    .thenBroadcastMutation(self => self.copy(value = "Changed"))

  val state: State[Entity1] = State(List(testStory))

}
case class Entity1(address:Address, value:String) extends Stateful {
  override protected val states: List[State[_ <: Stateful]] = List(Ent1.state)
  override protected var _currentState: State[_ <: Stateful] = states.head
}

