package puzzles.day17

import submarine.{Probe, Velocity}

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def numberOfVelocitiesReachingTarget: Int =
    Using.resource(Source.fromResource("day17.txt")) {
      file =>
        val targetArea = Probe.parseTargetArea(file.getLines().next())
        val (startX, endX, startY, endY) = Probe.velocityValues(targetArea)
        (startX to endX).flatMap(x =>
          (startY to endY).map(y => {
            val startingVelocity = Velocity(x, y)
            val maxYForThisVelocity = Probe.trajectory(Probe(0, 0, startingVelocity), targetArea)
            maxYForThisVelocity.map(_ => startingVelocity)
          })
        )
          .filterNot(_.isEmpty)
          .map(_.get)
          .toSet
          .size
    }

  Console.println(f"There are $numberOfVelocitiesReachingTarget distinct initial velocity values that reach the target.")
}
