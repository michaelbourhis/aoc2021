package puzzles.day15

import navigation.ChitonsCave

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def pathWithLowestRisk(multiplier: Int = 1): Long =
    Using.resource(Source.fromResource("day15.txt")) {
      file =>
        val lines = file.getLines()
        val grid = ChitonsCave.parse(lines)
        val (remaining, undiscovered) = ChitonsCave.init(grid, multiplier)
        ChitonsCave.findPath(remaining, undiscovered, grid, multiplier)
    }

  Console.println(f"The minimum risk is ${pathWithLowestRisk()}.")
}
