package navigation

case class Position(hPos: Int, depth: Int) {
  def coordinates: String = f"Horizontal position: $hPos, depth: $depth"

  def multipliedCoordinates: Int = hPos * depth
}

object Position {
  def move(current: Position, movement: Movement): Position = {
    movement match {
      case Forward(units) => Position(current.hPos + units, current.depth)
      case Up(units) => Position(current.hPos, current.depth - units)
      case Down(units) => Position(current.hPos, current.depth + units)
    }
  }
}
