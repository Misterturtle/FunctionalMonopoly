package top_layer

import Event.Event
import domain_models.DomainModel
import top_layer.Dependencies.TopObject

case object Story {
  def apply() = new Story(Nil)

  def runStory[A <: DomainModel](story: Story[A], domainModel: A): DomainModel = {
    story.eventStack.reverse.foldLeft(domainModel) {
      case (newState, eventFn) => {
        eventFn(newState).foldLeft(newState){
          case (newerState, event) => DomainModelGraph.updateDomainModel(newerState, event).asInstanceOf[A]
        }
      }
    }
  }
}

case class Story[A <: DomainModel](eventStack: List[DomainModel => List[Event]]) {

  def first(eventFn: DomainModel => Event): Story[A] = {copy(eventStack = { domainModel: DomainModel => List(eventFn(domainModel))} :: eventStack)}

  def first(eventFns: DomainModel => List[Event], jvmRequirement: Int = 0): Story[A] = andThen(eventFns)

  def andThen(eventFn: DomainModel => Event): Story[A] = copy(eventStack = {domainModel:DomainModel => List(eventFn(domainModel))} :: eventStack)

  def andThen(eventFns: DomainModel => List[Event], jvmRequirement: Int = 0): Story[A] = copy(eventStack = { eventFns :: eventStack })
}