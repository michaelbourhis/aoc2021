package navigation

case class AimedPosition(hPos: Int, depth: Int, aim: Int) {
  def coordinates: String = f"Horizontal position: $hPos, depth: $depth, aim: $aim"
  def multipliedCoordinates: Int = hPos * depth
}

object AimedPosition {
  def move(current: AimedPosition, movement: Movement): AimedPosition = {
    movement match {
      case Forward(units) => AimedPosition(current.hPos + units, current.depth + current.aim * units, current.aim)
      case Up(units) => AimedPosition(current.hPos, current.depth, current.aim - units)
      case Down(units) => AimedPosition(current.hPos, current.depth, current.aim + units)
    }
  }
}
