package story

import Implicits.MyImplicits._
import event_bus.{Address, AddressBook, EventBus, Subscriber}
import events.{QueryEvent, _}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.util.{Failure, Success, Try}



case class TestTriggerEvent() extends TriggerEvent

case object Entity1 {

  val testStory: Story[Entity1] = Story[Entity1]
    .whenTriggerEvent(typeTag[TestTriggerEvent])
    .thenBroadcastMutation(self => self.copy(value = "Changed"))

  val state: State[Entity1] = State(List(testStory))

  implicit val creationFn: Stateful[Entity1] = new Stateful[Entity1] {
    override val states: List[State[Entity1]] = List(state)
    override var _currentState: State[Entity1] = states.head
    override val address: Address = Address("root/entity1")
  }

}
case class Entity1(address:Address, value:String)


trait TypeRetention[A] {
  val tpe: TypeTag[A]
}

trait Stateful[A] {
  val _addressBook: AddressBook = AddressBook.shared
  val states: List[State[A]]
  var _currentState: State[A]
  val address: Address
}

case class State[A](stories: List[Story[A]]) {
  def enable(stateful: A): Unit = {
    stories.foreach(_.enable(stateful))
  }

  def disable: Unit = {
    stories.foreach(_.disable())
  }
}

trait TriggerStoryVerbage[A, B <: TriggerEvent] {
  def meetsCondition(fn: B => Boolean): TriggerStory[A, B]

  def thenBroadcastMutation(mutationFn: A => A): TriggerStory[A, B]
}

trait QueryStoryVerbage[A, B <: QueryEvent] {
  def respondsWith(fn: B => Boolean): QueryStory[A, B]
}

trait StoryVerbage[A] {
  def givenQueryEvent[B <: QueryEvent](queryEvent: B)(implicit tt:TypeTag[B]): QueryStory[A, B]

  def whenTriggerEvent[T <: TriggerEvent](eventClass: TypeTag[T]): TriggerStory[A, T]

  def enable(stateful: A, tt:TypeTag[A]): Unit

  def disable(): Unit
}

object Story {

  def apply[A] = new Story[A]()
}

class Story[A] extends StoryVerbage[A] with Subscriber {


  var eventBus: EventBus = EventBus.shared
  var addressBook: AddressBook = AddressBook.shared

  var _triggerCondition: Option[TriggerEvent => Boolean] = None
  var _queryCondition: Option[QueryEvent => Boolean] = None
  var _fireQueryEventWithResponse: Option[() => Future[_ <: QueryEvent]] = None
  var _triggerResult: Option[Future[Try[_ <: TriggerEvent]]] = None
  var _mutationFn: Option[A => A] = None
  var _triggerEventType: Option[TypeTag[_ <: TriggerEvent]] = None

  def givenQueryEvent[B <: QueryEvent](queryEvent: B)(implicit tt: TypeTag[B]): QueryStory[A, B] = {
    _fireQueryEventWithResponse = Some(() => {
      eventBus.post(queryEvent)
      eventBus.subscribeTo(tt, this)
    })

    QueryStory[A, B](this)
  }

  def whenTriggerEvent[T <: TriggerEvent](tt: TypeTag[T]): TriggerStory[A, T] = {

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

  def enable(stateful: A): Unit = {

    //todo: dangerous .get
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
            eventBus.postMutationEvent(mutationEvent)
            addressBook.updateStateful(stateful, mutationEvent)
          }

          case Failure(QueryConditionFailed()) => {
            this.disable()
            this.enable(stateful)
          }
        }
      }

      case Failure(TriggerConditionFailed()) => enable(stateful)
    }
  }

  def disable(): Unit = {
    _triggerEventType.map(eventBus.unsubscribe(_, this))
  }

}

case class TriggerConditionFailed[A]() extends Throwable

case class QueryConditionFailed[A]() extends Throwable

object QueryStory {
  def apply[A, B <: QueryEvent](baseStory: Story[A]) = new QueryStory[A, B](baseStory)
}

class QueryStory[A, B <: QueryEvent](val baseStory: Story[A]) extends Story[A] with QueryStoryVerbage[A, B] {
  def respondsWith(fn: B => Boolean): QueryStory[A, B] = {
    baseStory._queryCondition = Some(fn.asInstanceOf[QueryEvent => Boolean])
    this
  }
}

object TriggerStory {
  def apply[A, B <: TriggerEvent](baseStory: Story[A]) = new TriggerStory[A, B](baseStory)
}

class TriggerStory[A, B <: TriggerEvent](val baseStory: Story[A]) extends Story[A] with TriggerStoryVerbage[A, B] {

  def meetsCondition(fn: B => Boolean): TriggerStory[A, B] = {
    baseStory._triggerCondition = Some(fn.asInstanceOf[TriggerEvent => Boolean])
    this
  }

  def thenBroadcastMutation(mutationFn: A => A): TriggerStory[A, B] = {
    baseStory._mutationFn = Some(mutationFn)
    this
  }
}
