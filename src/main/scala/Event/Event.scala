package Event

import domain_models.DomainModel

trait Event {

  def run[A](domainModel:DomainModel[A]): DomainModel[A]

}
