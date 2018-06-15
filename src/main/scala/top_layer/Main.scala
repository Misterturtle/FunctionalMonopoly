package top_layer

import Input.{Input, InputHandler}
import domain_models.Game
import top_layer.DomainModelGraph.TopObject

object Main extends App{

  private var _topObject: TopObject = Game()
  val inputHandler = new InputHandler()

  def loop(topObject: TopObject): TopObject = {
    val input = inputHandler.getNextInput
    input match {
      case Some(story) => Story.runStory(story, topObject)
      case None => topObject
    }
  }

  def startProgram(): Unit = {
    val LOOP_DURATION = 100
    var startOfLoop:Long = 0

    while(startOfLoop - System.currentTimeMillis() < LOOP_DURATION){
      startOfLoop = System.currentTimeMillis()
      _topObject = loop(_topObject)
    }
  }

  startProgram()
}