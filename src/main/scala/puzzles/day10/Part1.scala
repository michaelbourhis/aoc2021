package puzzles.day10

import navigation.Subsystem

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def syntaxErrorScore: Long =
    Using.resource(Source.fromResource("day10.txt")) {
      file =>
        file
          .getLines()
          .map(Subsystem.parse)
          .map(_.score)
          .sum
    }

  Console.println(f"The total syntax error score is $syntaxErrorScore.")
}
