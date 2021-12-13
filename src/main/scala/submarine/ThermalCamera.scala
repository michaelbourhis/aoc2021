package submarine

case class Dot(x: Int, y:Int) {
  def foldHorizontally(position: Int): Dot =
    if(y<position) this
    else Dot(x, 2 * position - y)
  def foldVertically(position: Int): Dot =
    if(x<position) this
    else Dot(2 * position - x, y)
}
sealed abstract class Fold
case class HorizontalFold(position: Int) extends Fold
case class VerticalFold(position: Int) extends Fold

object ThermalCamera {
  def dotsAndInstructions(lines: Seq[String]): (Set[Dot], Seq[Fold]) = {
    val emptyLineIndex = lines.indexOf("")
    val (dots, folds) = lines.filterNot(_.equals("")).splitAt(emptyLineIndex)
    (
      dots.map(s =>
        s.split(",").map(_.toInt) match {
          case Array(x, y) => Dot(x, y)
        }
      ).toSet,
      folds.map(s => s.split(" ")(2).split("=") match {
        case Array("x", position) => VerticalFold(position.toInt)
        case Array("y", position) => HorizontalFold(position.toInt)
      })
    )
  }

}
