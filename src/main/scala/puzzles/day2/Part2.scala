package puzzles.day2

import navigation.{AimedPosition, Movement}

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def finalPosition: AimedPosition =
    Using.resource(Source.fromResource("day2.txt")) {
      file =>
        file
          .getLines()
          .map(Movement.parse)
          .foldLeft(originalPosition)(AimedPosition.move)
    }

  def originalPosition: AimedPosition = AimedPosition(0, 0, 0)

  Console.println(f"Multiplied coordinates: ${finalPosition.multipliedCoordinates}")
}
