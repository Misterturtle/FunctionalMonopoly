package event_bus

import events.{Event, EventType, QueryEvent}

import scala.collection.mutable

object EventBus {

  val shared:EventBus = EventBus()

  def apply():EventBus = new EventBus()

}


class EventBus() {
  private val subDictionary: SubDictionary = new SubDictionary()

  var queryFn: QueryEvent => Boolean = { event:QueryEvent => }

  def subscribersFor(eventClass:Class[_]): List[Subscriber] = subDictionary.subsFor(eventClass)

  def subscribeTo[T <: Event](eventClass: Class[T], subscriber: Subscriber): Unit = subDictionary.addSubFor(eventClass, subscriber)
}


class SubDictionary {

  private val dictionary = mutable.Map[Class[_], List[Subscriber]]()

  def addSubFor(eventClass: Class[_ <: Event], subscriber: Subscriber) = {
    if(!dictionary.isDefinedAt(eventClass)) dictionary(eventClass) = Nil

    dictionary(eventClass) = subscriber :: dictionary(eventClass)
  }

  def subsFor(eventClass: Class[_]) = dictionary(eventClass)
}