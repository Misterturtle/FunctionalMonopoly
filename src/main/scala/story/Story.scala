package story

import event_bus.{Address, AddressBook, EventBus, Subscriber}
import events.{QueryEvent, _}

import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe._


trait SF[A] {
  protected val states: List[State[Stateful]]
  protected var _currentState: State[Stateful]
}



object Stateful {
  def switchStates[A <: Stateful](stateful: A, newState: State[A])(implicit tt: TypeTag[A]): Unit = {
    stateful._currentState.stories.foreach(_.disable())
    stateful._currentState = newState.asInstanceOf[State[Stateful]]
    newState.stories.foreach(_.enable(stateful, tt))
  }
}

trait Stateful {

  protected val _addressBook: AddressBook = AddressBook.shared
  protected val states: List[State[_ <: Stateful]]
  protected var _currentState: State[_ <: Stateful]

  val address: Address

  def cast[A](tt: TypeTag[A]): A = this.asInstanceOf[A]

  _addressBook.initStateful(this)
  _currentState.enable(this)
  println(_addressBook.getStateful(address)._2.tpe)
}

case class State[A <: Stateful](stories: List[_ <: StoryVerbage[A]])(implicit tt: TypeTag[A]) {
  def enable(stateful: A): Unit = {
    stories.foreach(_.enable(stateful, tt))
  }

  def disable: Unit = {
    stories.foreach(_.disable())
  }
}


trait StoryExtension[A <: Stateful] extends StoryVerbage[A] {
  val baseStory: Story[A]

  def givenQueryEvent[B <: QueryEvent](queryEvent: B, tt: TypeTag[B]): QueryStory[A, B] = baseStory.givenQueryEvent(queryEvent, tt)

  def whenTriggerEvent[T <: TriggerEvent](tt: TypeTag[T]): TriggerStoryVerbage[A, T] = baseStory.whenTriggerEvent(tt)

  def enable(stateful: A, tt: TypeTag[A])(implicit mutationTT: TypeTag[MutationEvent[A]]): Unit = baseStory.enable(stateful, tt)

  def disable: Unit = baseStory.disable()
}

trait TriggerStoryVerbage[A <: Stateful, B <: TriggerEvent] extends StoryExtension[A] {
  def meetsCondition(fn: B => Boolean): TriggerStoryVerbage[A, B]

  def thenBroadcastMutation(mutationFn: A => A): TriggerStoryVerbage[A, B]
}

trait QueryStoryVerbage[A <: Stateful, B <: QueryEvent] extends StoryExtension[A] {
  def respondsWith(fn: B => Boolean): QueryStoryVerbage[A, B]
}

trait StoryVerbage[A <: Stateful] {
  def givenQueryEvent[B <: QueryEvent](queryEvent: B, tt: TypeTag[B]): QueryStoryVerbage[A, B]

  def whenTriggerEvent[T <: TriggerEvent](eventClass: TypeTag[T]): TriggerStoryVerbage[A, T]

  def enable(stateful: A, tt: TypeTag[A])(implicit mutationTT: TypeTag[MutationEvent[A]]): Unit

  def disable(): Unit
}

object Story {

  def apply[A <: Stateful] = new Story[A]()
}

class Story[A <: Stateful] extends StoryVerbage[A] with Subscriber {


  var eventBus: EventBus = EventBus.shared
  var addressBook: AddressBook = AddressBook.shared

  var _triggerCondition: Option[TriggerEvent => Boolean] = None
  var _queryCondition: Option[QueryEvent => Boolean] = None
  var _fireQueryEventWithResponse: Option[() => Future[_ <: QueryEvent]] = None
  var _triggerResult: Option[Future[Try[_ <: TriggerEvent]]] = None
  var _mutationFn: Option[A => A] = None
  var _triggerEventType: Option[TypeTag[_ <: TriggerEvent]] = None

  def givenQueryEvent[B <: QueryEvent](queryEvent: B, tt: TypeTag[B]): QueryStory[A, B] = {
    _fireQueryEventWithResponse = Some(() => {
      eventBus.post(queryEvent, tt)
      eventBus.subscribeTo(tt, this)
    })

    QueryStory[A, B](this)
  }

  def whenTriggerEvent[T <: TriggerEvent](tt: TypeTag[T]): TriggerStoryVerbage[A, T] = {

    _triggerEventType = Some(tt)

    TriggerStory[A, T](this)
  }

  private def testTriggerCondition(triggerEvent: TriggerEvent, condition: Option[TriggerEvent => Boolean]): Try[TriggerEvent] = {

    condition.map(conditionFn => conditionFn(triggerEvent)) match {
      case Some(true) => Success(triggerEvent)
      case Some(false) => Failure(TriggerConditionFailed())
      case None => Success(triggerEvent)
    }
  }

  private def testQueryCondition(queryResponse: QueryEvent, condition: Option[QueryEvent => Boolean]): Try[QueryEvent] = {

    condition.map(conditionFn => conditionFn(queryResponse)) match {
      case Some(true) => Success(queryResponse)
      case Some(false) => Failure(QueryConditionFailed())
      case None => Success(queryResponse)
    }
  }

  def enable(stateful: A, tt: TypeTag[A])(implicit mutationTT: TypeTag[MutationEvent[A]]): Unit = {

    val triggerFuture = eventBus.subscribeTo(_triggerEventType.get, this)

    val triggerAfterCondition: Future[Try[TriggerEvent]] = triggerFuture.map { case triggerEvent: TriggerEvent =>
      testTriggerCondition(triggerEvent, _triggerCondition)
    }

    triggerAfterCondition.map {
      case Success(_) => {
        val queryFuture = _fireQueryEventWithResponse.get()

        val queryAfterCondition = queryFuture.map { case queryEvent: QueryEvent =>
          testQueryCondition(queryEvent, _queryCondition)
        }

        queryAfterCondition.map {
          case Success(queryEvent: QueryEvent) => {
            //todo: Dangerous .get
            val mutationEvent = MutationEvent[A](_mutationFn.get)
            eventBus.post(mutationEvent, mutationTT)
            addressBook.updateStateful(stateful, mutationEvent, tt)
          }

          case Failure(QueryConditionFailed()) => {
            this.disable()
            this.enable(stateful, tt)
          }
        }
      }

      case Failure(TriggerConditionFailed()) => enable(stateful, tt)
    }
  }

  def disable(): Unit = {
    _triggerEventType.map(eventBus.unsubscribe(_, this))
  }
}

case class TriggerConditionFailed[A]() extends Throwable

case class QueryConditionFailed[A]() extends Throwable

object QueryStory {
  def apply[A <: Stateful, B <: QueryEvent](baseStory: Story[A]) = new QueryStory[A, B](baseStory)
}

class QueryStory[A <: Stateful, B <: QueryEvent](val baseStory: Story[A]) extends QueryStoryVerbage[A, B] with StoryExtension[A] {
  def respondsWith(fn: B => Boolean): QueryStoryVerbage[A, B] = {
    baseStory._queryCondition = Some(fn.asInstanceOf[QueryEvent => Boolean])
    this
  }
}

object TriggerStory {
  def apply[A <: Stateful, B <: TriggerEvent](baseStory: Story[A]) = new TriggerStory[A, B](baseStory)
}

class TriggerStory[A <: Stateful, B <: TriggerEvent](val baseStory: Story[A]) extends TriggerStoryVerbage[A, B] with StoryExtension[A] {

  def meetsCondition(fn: B => Boolean): TriggerStoryVerbage[A, B] = {
    baseStory._triggerCondition = Some(fn.asInstanceOf[TriggerEvent => Boolean])
    this
  }

  def thenBroadcastMutation(mutationFn: A => A): TriggerStoryVerbage[A, B] = {
    baseStory._mutationFn = Some(mutationFn)
    this
  }
}
