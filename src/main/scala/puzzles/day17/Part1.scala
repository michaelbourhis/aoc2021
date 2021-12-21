package puzzles.day17

import submarine.Probe

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def maxYReached: Int =
    Using.resource(Source.fromResource("day17.txt")) {
      file =>
        val targetArea = Probe.parseTargetArea(file.getLines().next())
        val optimumVelocity = Probe.optimumVelocity(targetArea)
        optimumVelocity * (optimumVelocity + 1) / 2
    }

  Console.println(f"The max Y reached is $maxYReached.")
}
