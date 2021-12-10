package puzzles.day10

import navigation.Subsystem

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def completionScore: Long =
    Using.resource(Source.fromResource("day10.txt")) {
      file =>
        val completionScores = file
          .getLines()
          .map(Subsystem.parse)
          .filterNot(_.illegal)
          .map(_.completeLine)
          .toSeq
          .sorted
        completionScores(completionScores.length / 2)
    }

  Console.println(f"The completion score is $completionScore.")
}
