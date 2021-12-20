package submarine

import utils.BitsConversion.bitsToInt

object ImageEnhancer {
  type EnhancementAlgorithm = Seq[Boolean]
  type ImageLine = Seq[Boolean]
  type Image = Seq[ImageLine]

  def parse(lines: Iterator[String], steps: Int): (EnhancementAlgorithm, Image) = {
    val enhancementAlgorithm = toPixel(lines.next())
    lines.next()
    val image = lines.toSeq.map(toPixel)
    val imageFilledWithDark =
      (0 until 2 * steps).map(_ => darkLine(image.length + 4 * steps)) ++
        image.map(line => darkLine(2 * steps) ++ line ++ darkLine(2 * steps)) ++
        (0 until 2 * steps).map(_ => darkLine(image.length + 4 * steps))
    (enhancementAlgorithm, imageFilledWithDark)
  }

  def toPixel(str: String): Seq[Boolean] = str.toCharArray.map {
    case '.' => false
    case '#' => true
  }.toSeq

  def enhance(enhancementAlgorithm: EnhancementAlgorithm, inputImage: Image): Image = {
    val outputImage = (1 until inputImage.length - 1).map(i =>
      (1 until inputImage.head.length - 1).map(j => {
        applyAlgorithm(
          Seq(
            inputImage(i - 1)(j - 1), inputImage(i - 1)(j), inputImage(i - 1)(j + 1),
            inputImage(i)(j - 1), inputImage(i)(j), inputImage(i)(j + 1),
            inputImage(i + 1)(j - 1), inputImage(i + 1)(j), inputImage(i + 1)(j + 1)
          ),
          enhancementAlgorithm
        )
      })
    )
    Console.println()
    outputImage.foreach(line => {
      line.foreach(pixel => Console.print(if (pixel) "#" else "."))
      Console.println()
    })
    Console.println()
    outputImage
  }

  def darkLine(size: Int): ImageLine =
    Seq.fill(size)(false)

  def applyAlgorithm(pixels: Seq[Boolean], enhancementAlgorithm: EnhancementAlgorithm): Boolean = {
    val position = bitsToInt(pixels)
    enhancementAlgorithm(position)
  }
}
