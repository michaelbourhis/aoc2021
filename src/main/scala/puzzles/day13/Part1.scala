package puzzles.day13

import submarine.{HorizontalFold, ThermalCamera, VerticalFold}

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def visibleDotsAfterFirstFold: Int =
    Using.resource(Source.fromResource("day13.txt")) {
      file =>
        val lines = file.getLines().toSeq
        val (dots, folds) = ThermalCamera.dotsAndInstructions(lines)
        val dotsAfterFirstFold = folds.head match {
          case HorizontalFold(position) => dots.filterNot(_.y == position).map(_.foldHorizontally(position))
          case VerticalFold(position) => dots.filterNot(_.x == position).map(_.foldVertically(position))
        }
        dotsAfterFirstFold.size
    }

  Console.println(f"There will be $visibleDotsAfterFirstFold visible dots after first fold.")
}
