package domain_models

import top_layer.DomainModelGraph

trait DomainModel[A] {

  def getGenericType : Class[A] = Class[A]

  def updateChild(child: DomainModel[_]): DomainModel[A]

}
