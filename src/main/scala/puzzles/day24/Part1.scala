package puzzles.day24

import submarine.ALU

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def maxMonad: Long =
    Using.resource(Source.fromResource("day24.txt")) {
      file =>
        val operations = ALU.parse(file.getLines())
        ALU.resolve(operations)
    }

  Console.println(f"Max MONAD is: $maxMonad.")
}
