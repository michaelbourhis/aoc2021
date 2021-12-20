package utils

object BitsConversion {
  def bitsToInt(bits: Seq[Boolean]): Int =
    bits.foldLeft(0)(
      (i, b) => (i << 1) + (if (b) 1 else 0)
    )

  def bitsToLong(bits: Seq[Boolean]): Long =
    bits.foldLeft(0L)(
      (i, b) => (i << 1) + (if (b) 1 else 0)
    )
}
