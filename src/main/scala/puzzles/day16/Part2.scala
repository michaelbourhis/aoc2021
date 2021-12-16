package puzzles.day16

import submarine.Bits

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def valueOfOutermostPacket: Long =
    Using.resource(Source.fromResource("day16.txt")) {
      file =>
        val transmission = file.getLines().next()
        val bits = Bits.parseTransmission(transmission)
        val packets = Bits.readPackets(bits)
        packets
          ._1
          .head
          .apply
    }

  Console.println(f"The value of the outermost packet is $valueOfOutermostPacket.")
}
