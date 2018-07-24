package event_bus

import events.{Event, QueryEvent, TriggerEvent}

import scala.concurrent.Future
import scala.util.Failure
//import story.TriggerState

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Promise
import scala.util.Success
import scala.reflect.runtime.universe.{TypeTag, typeTag}

case class UnknownPromise(promise:Promise[_]) extends Throwable

trait Subscriber {

  private val eventBus = EventBus.shared
  private var _queryPromise : Promise[QueryEvent] = _
  private var _triggerPromise : Promise[TriggerEvent] = _
  private var _repeatTriggerPromise : Boolean = false


  def promiseEvent[A](promise:Promise[A], repeat: Boolean = false) = {

    promise

    promise match {
      case typedPromise : Promise[TriggerEvent] => {
        _triggerPromise = typedPromise
        _repeatTriggerPromise = repeat
      }
      case typedPromise : Promise[QueryEvent] => _queryPromise = typedPromise
      case unknownPromise : Promise[_] => unknownPromise.failure(UnknownPromise(unknownPromise))
    }
  }

  def fulfillEventPromise[B <: Event](event:B, tt: TypeTag[B]): Unit = {

    event match {
      case queryEvent: QueryEvent => {
        _queryPromise.success(queryEvent)
      }

      case triggerEvent: TriggerEvent => {
        _triggerPromise.success(triggerEvent)

        if(_repeatTriggerPromise){
          eventBus.subscribeTo(tt, this)
        }
      }

      case _ => throw new Exception("Unknown event type. Subscriber.fulfillEventPromise("+event+")")
    }
  }
}
