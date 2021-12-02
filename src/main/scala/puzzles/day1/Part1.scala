package puzzles.day1

import utils.IntegerString

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def numberOfIncreases: Int =
    Using.resource(Source.fromResource("day1.txt")) {
      file =>
        file
          .getLines()
          .sliding(2)
          .map(
            lines => {
              val previousDepth = IntegerString.parse(lines.head)
              val currentDepth = IntegerString.parse(lines.last)
              if (previousDepth < currentDepth) 1 else 0
            }
          ).sum
    }

  Console.println(f"Found $numberOfIncreases increases in measure.")
}
