package puzzles.day7

import animals.Crab

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def minimumRequiredFuel: Int =
    Using.resource(Source.fromResource("day7.txt")) {
      file =>
        val initialCrabPositions = file
          .getLines()
          .map(Crab.initialPositions)
          .next()
        val optimalPosition = Crab.optimalPositionToAlign(initialCrabPositions, expensiveMove = true)
        Crab.totalFuelNeededToAlignTo(initialCrabPositions, optimalPosition, expensiveMove = true)
    }

  Console.println(f"Crabs will use a minimum of $minimumRequiredFuel fuel to align.")
}
