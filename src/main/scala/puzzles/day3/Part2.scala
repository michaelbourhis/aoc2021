package puzzles.day3

import submarine.Diagnostic

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def lifeSupportRating: Int =
    Using.resource(Source.fromResource("day3.txt")) {
      file =>
        val (beginsWithZero, beginsWithOne) = file
          .getLines()
          .map(Diagnostic.getBits)
          .foldLeft(Diagnostic.initZerosAndOnes)(Diagnostic.separateZerosAndOnes(0))
        Diagnostic.lifeSupportRating(beginsWithZero, beginsWithOne)
    }

  Console.println(f"Life support rating: $lifeSupportRating.")
}
