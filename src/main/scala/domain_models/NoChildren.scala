package domain_models

trait NoChildrenDomainModel extends DomainModel[_] {
  override def updateChild(child: DomainModel[_]): DomainModel[_] = {
    //This will never get called
    throw new Exception("Attempting to update a DomainModel with no children. Received child: "+ child)
  }
}
