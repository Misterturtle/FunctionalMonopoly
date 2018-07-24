package event_bus

import events.MutationEvent
import story.Stateful

import scala.collection.mutable
import scala.reflect.runtime.universe._

case class Address(path:String)

object AddressBook {
  val shared = new AddressBook()
}

class AddressBook(){

  private var lookup: mutable.Map[String, (Stateful, TypeTag[_])] = mutable.Map[String, (Stateful, TypeTag[_])]()

  def getStateful(address:Address):(Stateful, TypeTag[_]) = {
    lookup(address.path)
  }

  def updateStateful[A <: Stateful](stateful:A, mutationEvent: MutationEvent[A])(implicit tag :TypeTag[A]) = {
    lookup(stateful.address.path) = (mutationEvent.mutationFn(stateful), tag)
  }


}