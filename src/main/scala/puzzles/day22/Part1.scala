package puzzles.day22

import submarine.Reactor

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def numberOfCubesOn: BigInt =
    Using.resource(Source.fromResource("day22.txt")) {
      file =>
        val rebootSteps = file
          .getLines()
          .map(Reactor.parseRebootStep)
          .map(_.filterOutOfGrid(-50, 50, -50, 50, -50, 50))
          .filterNot(_.cuboid.emptyCuboid)
          .toSeq
        Reactor.executeSteps(rebootSteps)
    }

  Console.println(f"After the reboot procedure, $numberOfCubesOn cubes are on.")
}
