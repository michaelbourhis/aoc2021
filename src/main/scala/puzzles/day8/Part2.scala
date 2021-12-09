package puzzles.day8

import submarine.Display

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def guessOutputsAndSum: Int =
    Using.resource(Source.fromResource("day8.txt")) {
      file =>
        file
          .getLines()
          .map(Display.parse)
          .map(Display.guessDigits)
          .map(note =>
            note.output.map(d => note.digitMapping(d)).mkString.toInt
          )
          .sum
    }

  Console.println(f"The sum of the outputs should be $guessOutputsAndSum.")
}
