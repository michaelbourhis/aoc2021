package puzzles.day18

import animals.{Pair, SnailFish}

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def maxMagnitudeOfASum: Int =
    Using.resource(Source.fromResource("day18.txt")) {
      file =>
        val problems = file.getLines().toSeq
          .map(SnailFish.parseProblem)
          .map(SnailFish.reduceIt)
        (0 until problems.length - 1).flatMap(idx1 => {
          (idx1 + 1 until problems.length).flatMap(idx2 => {
            val xPlusY = Pair(problems(idx1), problems(idx2))
            val yPlusX = Pair(problems(idx2), problems(idx1))
            Seq(
              SnailFish.reduceIt(xPlusY).magnitude(),
              SnailFish.reduceIt(yPlusX).magnitude()
            )
          })
        }).max
    }

  Console.println(f"The total magnitude is $maxMagnitudeOfASum")
}
