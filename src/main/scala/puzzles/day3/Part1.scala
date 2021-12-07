package puzzles.day3

import submarine.Diagnostic

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def powerConsumption: Int =
    Using.resource(Source.fromResource("day3.txt")) {
      file =>
        val byteDistributions = file
          .getLines()
          .map(Diagnostic.getBits)
          .foldLeft(Diagnostic.initBitDistributions)(Diagnostic.parse)
        Diagnostic.powerConsumption(byteDistributions)
    }

  Console.println(f"Power consumption of the submarine: $powerConsumption.")
}
