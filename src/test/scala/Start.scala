import org.scalatest.{FreeSpec, Matchers}

class Start extends FreeSpec with Matchers {

  val emptyStory = Story[Game]

  "A game is started with 4 players" - {

    val game = Story[Game].first(initGame)

    val result = Story.runStory(game, Game())

    result.players.size shouldBe 4
  }



}
