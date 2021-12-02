package navigation

import utils.IntegerString

import scala.util.matching.Regex

sealed trait Movement {
  def units: Int
}

case class Forward(units: Int) extends Movement

case class Up(units: Int) extends Movement

case class Down(units: Int) extends Movement

object Movement {
  val commandPattern: Regex = raw"(forward|up|down) (\d+)".r

  def parse(command: String): Movement = {
    command match {
      case commandPattern("forward", units) => Forward(IntegerString.parse(units))
      case commandPattern("up", units) => Up(IntegerString.parse(units))
      case commandPattern("down", units) => Down(IntegerString.parse(units))
      case _ =>
        Console.println(f"Unable to parse command: $command.")
        sys.exit(1)
    }
  }
}
