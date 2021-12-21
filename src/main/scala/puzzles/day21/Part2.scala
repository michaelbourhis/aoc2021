package puzzles.day21

import submarine.{DiracDice, Universe}

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def numberOfUniversesInWhichBestPlayerWins: BigInt =
    Using.resource(Source.fromResource("day21.txt")) {
      file =>
        val players = file
          .getLines()
          .map(DiracDice.parse)
          .toSeq
        val startingUniverse = Universe(players.head, players.last)
        val gameResults = DiracDice.playWithDiracDice(Seq((startingUniverse, BigInt(1))))
        gameResults
          .values
          .max
    }

  Console.println(f"After the game ends, the score of losing player multiplied by the number of times the die was rolled is: $numberOfUniversesInWhichBestPlayerWins")
}
