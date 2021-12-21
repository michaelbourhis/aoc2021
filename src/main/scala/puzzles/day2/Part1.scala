package puzzles.day2

import navigation.{Movement, Position}

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def finalPosition: Position =
    Using.resource(Source.fromResource("day2.txt")) {
      file =>
        file
          .getLines()
          .map(Movement.parse)
          .foldLeft(originalPosition)(Position.move)
    }

  def originalPosition: Position = Position(0, 0)

  Console.println(f"Multiplied coordinates: ${finalPosition.multipliedCoordinates}")
}
