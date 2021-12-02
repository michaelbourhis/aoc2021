package utils

import scala.util.Try

object IntegerString {
  def parse(str: String): Int =
    Try(str.toInt).getOrElse {
      Console.println(f"Encountered a non integer: $str.")
      sys.exit(1)
    }
}
