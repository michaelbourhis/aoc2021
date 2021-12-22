package puzzles.day22

import submarine.Reactor

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def numberOfCubesOn: BigInt =
    Using.resource(Source.fromResource("day22.txt")) {
      file =>
        val rebootSteps = file
          .getLines()
          .map(Reactor.parseRebootStep)
          .toSeq
        Reactor.executeSteps(rebootSteps)
    }

  Console.println(f"After the reboot procedure, $numberOfCubesOn cubes are on.")
}
