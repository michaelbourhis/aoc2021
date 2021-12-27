package puzzles.day24

import submarine.ALU

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def minMonad: Long =
    Using.resource(Source.fromResource("day24.txt")) {
      file =>
        val operations = ALU.parse(file.getLines())
        ALU.resolve(operations, getSmallest = true)
    }

  Console.println(f"Smallest MONAD is: $minMonad.")
}
