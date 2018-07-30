package event_bus

import events.{Event, MutationEvent}
import story.TypeRetention

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

  def postMutationEvent[A](mutationEvent: MutationEvent[A]): Unit = {
    eventStack.addEvent(mutationEvent)
    notifySubsOfEvent(mutationEvent, mutationEvent.tt)
  }

  def post[B <: Event : TypeRetention[B]](event:B): Unit = {
    eventStack.addEvent(event)
    notifySubsOfEvent(event, event.tpe)
  }

  private def notifySubsOfEvent[B <: Event : TypeRetention[B]](event:B) = {
    val subs = subDictionary.subsFor(event.tpe)
    subs.foreach(_.fulfillEventPromise(event))
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