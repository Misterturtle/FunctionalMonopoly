package Implicits

import event_bus.{Address, AddressBook}
import story.{State, Stateful}
import scala.reflect.runtime.universe.{TypeTag, typeTag}

object MyImplicits {

  implicit class StatefulOps[A](a: A)(implicit ev: Stateful[A]) {

    val _addressBook: AddressBook = ev._addressBook
    val states: List[State[A]] = ev.states
    var _currentState: State[A] = ev._currentState
    val address: Address = ev.address

    ev._addressBook.initStateful(a)
    _currentState.enable(a)
    println(ev._addressBook.getStateful(address))



    /*
    def switchStates[A <: Stateful](stateful: A, newState: State[A])(implicit tt: TypeTag[A]): Unit = {
    stateful._currentState.stories.foreach(_.disable())
    stateful._currentState = newState.asInstanceOf[State[Stateful]]
    newState.stories.foreach(_.enable(stateful, tt))
  }

     */

  }


  implicit class TypeRetentionOps[A](a:A)(implicit ev: TypeTag[A]) {
    val tpe: TypeTag[A] = ev
  }


}