package puzzles.day16

import submarine.Bits

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def sumOfPacketVersions: Long =
    Using.resource(Source.fromResource("day16.txt")) {
      file =>
        val transmission = file.getLines().next()
        val bits = Bits.parseTransmission(transmission)
        val packets = Bits.readPackets(bits)
        packets
          ._1
          .map(_.totalVersion)
          .sum
    }

  Console.println(f"The sum of packet versions is $sumOfPacketVersions.")
}
