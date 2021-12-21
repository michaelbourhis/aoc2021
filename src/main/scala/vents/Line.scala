package vents

case class Line(points: Seq[Point])

case class Point(x: Int, y: Int)

case class MapOfThePoints(points: Map[Point, Int]) {
  def dangerousPoints(dangerLevel: Int): Seq[Point] = {
    points.filter(_._2 >= dangerLevel).keys.toSeq
  }
}

object Line {
  def parseAllLines(representation: String): Line = {
    val line = parseHorizontalAndVertical(representation)
    if (line.points.isEmpty) {
      val Seq(start, end) = startAndEndPoints(representation)
      val stepX = if (start.x <= end.x) 1 else -1
      val stepY = if (start.y <= end.y) 1 else -1
      Line((0 to (end.x - start.x) * stepX).map(i => Point(start.x + i * stepX, start.y + i * stepY)))
    }
    else line
  }

  def startAndEndPoints(representation: String): Seq[Point] =
    representation
      .split(" -> ")
      .map(_.split(",").map(_.toInt))
      .map(p => Point(p(0), p(1)))

  def parseHorizontalAndVertical(representation: String): Line = {
    val Seq(start, end) = startAndEndPoints(representation)
    if (start.x == end.x) {
      val step = if (start.y <= end.y) 1 else -1
      Line((start.y to end.y by step).map(Point(start.x, _)))
    }
    else if (start.y == end.y) {
      val step = if (start.x <= end.x) 1 else -1
      Line((start.x to end.x by step).map(Point(_, start.y)))
    }
    else
      Line(Seq.empty)
  }

  def initMapOfPoints: MapOfThePoints = MapOfThePoints(Map.empty[Point, Int])

  def fillMapOfPoints(mapOfThePoints: MapOfThePoints, newLine: Line): MapOfThePoints = {
    MapOfThePoints(
      (mapOfThePoints.points.keys ++ newLine.points).map(point => {
        val existingPointVentLevel = mapOfThePoints.points.getOrElse(point, 0)
        val newPointVentLevel = newLine.points.find(_.equals(point)).map(_ => 1).getOrElse(0)
        (point, existingPointVentLevel + newPointVentLevel)
      }).toMap
    )
  }
}
