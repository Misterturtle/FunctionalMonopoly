package given

import event_bus.{Address, AddressBook, EventBus}
import org.scalatest.{FreeSpec, Matchers}
import story.{Entity1, TestTriggerEvent}

import Implicits.MyImplicits._
import scala.reflect.runtime.universe._

class EventBusTests extends FreeSpec with Matchers {






  "playground" in {

    val eventBus = EventBus.shared
    val addressBook = AddressBook.shared

    val entity = Entity1(Address("/root"), "Not Change")

    eventBus.post(TestTriggerEvent())

    val newEntity = addressBook.getStateful(Address("/root"))._1.asInstanceOf[Entity1]

    newEntity.value shouldBe "Changed"
  }


}





