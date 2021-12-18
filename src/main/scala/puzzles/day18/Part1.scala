package puzzles.day18

import animals.{Pair, Snailfish}

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def magnitude: Int =
    Using.resource(Source.fromResource("day18.txt")) {
      file =>
        val problems = file.getLines().toSeq
          .map(Snailfish.parseProblem)
          .map(Snailfish.reduceIt)
        problems.tail
          .foldLeft(problems.head)((intermediateResult, problem) => {
            val newPair = Pair(intermediateResult, problem)
            Snailfish.reduceIt(newPair)
          })
          .magnitude()
    }

  Console.println(f"The total magnitude is $magnitude")
}
