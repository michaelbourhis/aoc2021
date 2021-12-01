package utils

import scala.util.Try

object Depth {
  def parse(str: String): Int =
    Try(str.toInt).getOrElse {
      Console.println(f"Encountered a non integer depth: $str.")
      sys.exit(1)
    }
}
