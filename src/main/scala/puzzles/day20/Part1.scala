package puzzles.day20

import submarine.ImageEnhancer

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def numberOfLitPixelsAfterSteps(steps: Int): Int =
    Using.resource(Source.fromResource("day20.txt")) {
      file =>
        val (enhancementAlgorithm, inputImage) = ImageEnhancer.parse(file.getLines(), steps)
        val outputImage = (0 until steps).foldLeft(inputImage)((image, _) => ImageEnhancer.enhance(enhancementAlgorithm, image))
        outputImage.flatten.count(identity)
    }

  Console.println(f"The number of lit pixels is ${numberOfLitPixelsAfterSteps(2)}")
}
