package puzzles.day25

import animals.SeaCucumber

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def numberOfStepsUntilTheyStop: Int =
    Using.resource(Source.fromResource("day25.txt")) {
      file =>
        val seaFloor = SeaCucumber.parse(file.getLines())
        SeaCucumber.stepsUntilItStops(seaFloor)
    }

  Console.println(f"Sea cucumbers will stop after $numberOfStepsUntilTheyStop steps.")
}
