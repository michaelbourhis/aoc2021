package puzzles.day21

import submarine.{DeterministicDice, DiracDice}

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def scoreOfLosingPlayerByTimesDiceRolled: Int =
    Using.resource(Source.fromResource("day21.txt")) {
      file =>
        val players = file
          .getLines()
          .map(DiracDice.parse)
          .toSeq
        val dice = DeterministicDice()
        val (gameResult, finalDice) = DiracDice.playWithDeterministicDice(players.head, players.last, dice)
        gameResult.loser.score * finalDice.timesRolled
    }

  Console.println(f"After the game ends, the score of losing player multiplied by the number of times the die was rolled is: $scoreOfLosingPlayerByTimesDiceRolled")
}
