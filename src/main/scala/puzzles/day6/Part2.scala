package puzzles.day6

import fish.LanterFish

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  val numberOfDays = 256

  def numberOfFishAtTheEnd: Long =
    Using.resource(Source.fromResource("day6.txt")) {
      file =>
        val initialFishState = file
          .getLines()
          .map(LanterFish.initCycle)
          .next()
        LanterFish
          .reproduction(initialFishState, numberOfDays)
          .values
          .sum
    }

  Console.println(f"There will be $numberOfFishAtTheEnd fish after $numberOfDays days.")
}
