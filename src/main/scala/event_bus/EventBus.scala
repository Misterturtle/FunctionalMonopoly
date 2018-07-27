package event_bus

import events.Event

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.reflect.runtime.universe._

object EventBus {

  var shared:EventBus = EventBus()

  def apply():EventBus = new EventBus()
}


class EventBus() {
  private val subDictionary: SubDictionary = new SubDictionary()

  private val eventStack: EventStack = new EventStack()

  def subscribersFor(tt:TypeTag[_ <: Event]): List[Subscriber] = subDictionary.subsFor(tt.tpe)

  def subscribeTo[B <: Event](tt: TypeTag[B], subscriber: Subscriber): Future[B] = {
    val promise = subDictionary.addSubFor(tt.tpe, subscriber)
    subscriber.promiseEvent(promise)
    promise.future
  }

  def unsubscribe(tt: TypeTag[_ <: Event], subscriber: Subscriber): Unit = {
    subDictionary.removeSubFor(tt.tpe, subscriber)
  }

  def post[B <: Event](event:B, tt: TypeTag[B]): Unit = {
    eventStack.addEvent(event)
    notifySubsOfEvent(event, tt)
  }

  private def notifySubsOfEvent[B <: Event](event:B, tt:TypeTag[B]) = {
    val subs = subDictionary.subsFor(tt.tpe)
    subs.foreach(_.fulfillEventPromise(event, tt))
  }
}


class SubDictionary {

  private val dictionary = mutable.Map[Type, List[Subscriber]]()

  def addSubFor[B <: Event](tt: Type, subscriber: Subscriber): Promise[B] = {
    if(!dictionary.isDefinedAt(tt)) dictionary(tt) = Nil

    dictionary(tt) = subscriber :: dictionary(tt)

    Promise[B]
  }

  def removeSubFor(event: Type, subscriber: Subscriber): Unit = {
    dictionary(event) = subsFor(event).filter(_ == subscriber)
  }

  def subsFor[A <: Event](tt: Type): List[Subscriber] =
    if(dictionary.isDefinedAt(tt)){
      dictionary(tt)
    } else {
      Nil
    }
}

class EventStack {

  private val stack = ListBuffer[Event]()

  def addEvent(event:Event) = stack.append(event)
}