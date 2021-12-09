package puzzles.day8

import submarine.Display

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def numberOfTimesObviousDigitsAppear: Int =
    Using.resource(Source.fromResource("day8.txt")) {
      file =>
        file
          .getLines()
          .map(Display.parse)
          .map(note =>
            note.output.count(digit => Display.obviousSizes.contains(digit.segments.length))
          )
          .sum
    }

  Console.println(f"Digits 1, 4, 7, and 8 appear $numberOfTimesObviousDigitsAppear times.")
}
