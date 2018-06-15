package domain_models

import top_layer.DomainModelGraph.TopObject

case class Game(players:List[Player]) extends DomainModel[TopObject]{

  override def updateChild(child: DomainModel[_]): DomainModel[TopObject] = {
    child match {
      case player:Player => copy(player :: players.filterNot(_.id == player.id))
    }
  }
}
