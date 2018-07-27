package event_bus

import events.MutationEvent
import story.Stateful

import scala.collection.mutable
import scala.concurrent.Future
import scala.reflect.runtime.universe._

case class Address(path:String)

object AddressBook {
  val shared = new AddressBook()
}

class AddressBook(){

  private var lookup: mutable.Map[String, (Stateful, TypeTag[_])] = mutable.Map[String, (Stateful, TypeTag[_])]()

  //todo: Return back a Future that will fulfill when the stateful can be async updated
  def getStateful(address:Address): (Stateful, TypeTag[_]) = {
    lookup(address.path)
  }

  //todo: Return back a Future that will fulfill when the stateful can be async updated
  def updateStateful[A <: Stateful](stateful:A, mutationEvent: MutationEvent[A], tt :TypeTag[A]): Unit = {
    lookup(stateful.address.path) = (mutationEvent.mutationFn(stateful), tt)
  }

  def initStateful[A <: Stateful](stateful: A)(implicit tag: TypeTag[A]): Unit = {
    lookup(stateful.address.path) = (stateful, tag)
  }


}