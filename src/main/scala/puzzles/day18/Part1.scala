package puzzles.day18

import animals.{Pair, SnailFish}

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def magnitude: Int =
    Using.resource(Source.fromResource("day18.txt")) {
      file =>
        val problems = file.getLines().toSeq
          .map(SnailFish.parseProblem)
          .map(SnailFish.reduceIt)
        problems.tail
          .foldLeft(problems.head)((intermediateResult, problem) => {
            val newPair = Pair(intermediateResult, problem)
            SnailFish.reduceIt(newPair)
          })
          .magnitude()
    }

  Console.println(f"The total magnitude is $magnitude")
}
