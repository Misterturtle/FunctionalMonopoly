package event_bus

import Implicits.MyImplicits._
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

  private var lookup: mutable.Map[String, (A, Stateful[A]) forSome { type A }] = mutable.Map[String, (A, Stateful[A]) forSome { type A }]()

  //todo: Return back a Future that will fulfill when the stateful can be async updated
  def getStateful(address:Address): (A, Stateful[A]) forSome { type A } = {
    lookup(address.path)
  }

  //todo: Return back a Future that will fulfill when the stateful can be async updated
  def updateStateful[A : Stateful](stateful:A, mutationEvent: MutationEvent[A]): Unit = {
    lookup(stateful.address.path) = (mutationEvent.mutationFn(stateful), implicitly[Stateful[A]])
  }

  def initStateful[A : Stateful](stateful: A): Unit = {
    lookup(stateful.address.path) = (stateful, implicitly[Stateful[A]])
  }
}