package top_layer

import Event.{ErrorEvent, Event, OutputEvent, SideEffectEvent}
import domain_models.{Child, DomainModel}

import scala.collection.mutable
import scala.util.control.TailCalls.TailRec

object DomainModelGraph {

  val parentLookup = mutable.Map[DomainModel[DomainModel[_]], DomainModel[_]]()


  private var topObject: Option[TopObject] = None

  abstract class TopObject extends DomainModel[Unit] {
    override def updateChild(child: DomainModel[_]): DomainModel[Unit] = {
      topObject = Some(this)
      this
    }
  }


  def updateDomainModel[A <: DomainModel[_]](domainModel: DomainModel[A], event: Event): Unit = {
    val newDomainModel = event.run(domainModel)
    go(newDomainModel)

    @TailRec
    def go(domainModel: DomainModel[_]): Unit = {
      val domainParent = parentLookup(domainModel.asInstanceOf[DomainModel[DomainModel[_]]])
      if (domainParent.isInstanceOf[TopObject]) {
        domainParent.updateChild(newDomainModel)
      } else {
        go(domainParent.updateChild(newDomainModel).asInstanceOf[DomainModel[A]])
      }
    }

  }
}