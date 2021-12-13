package puzzles.day13

import submarine.{Dot, HorizontalFold, ThermalCamera, VerticalFold}

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def whatDotsLookLike: String =
    Using.resource(Source.fromResource("day13.txt")) {
      file =>
        val lines = file.getLines().toSeq
        val (initialDots, folds) = ThermalCamera.dotsAndInstructions(lines)
        val dotsAfterAllFolds = folds.foldLeft(initialDots)((dots, fold) => {
          fold match {
            case HorizontalFold(position) => dots.filterNot(_.y == position).map(_.foldHorizontally(position))
            case VerticalFold(position) => dots.filterNot(_.x == position).map(_.foldVertically(position))
          }
        })
        val maxX = dotsAfterAllFolds.maxBy(_.x).x
        val maxY = dotsAfterAllFolds.maxBy(_.y).y
        (0 to maxY).map(y =>
          (0 to maxX).map(x =>
            if (dotsAfterAllFolds.contains(Dot(x, y))) " @ " else "   "
          ).mkString
        ).mkString("\n")
    }

  Console.println(f"Here is what the paper looks like after all folds:\n$whatDotsLookLike")
}
