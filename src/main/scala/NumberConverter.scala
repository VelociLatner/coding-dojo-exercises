
object NumberConverter extends (Long => String) {

  val NULL_CONVERTER = new NumberConverter(0, null, null) {
    override def convert(x: String) = ""
  }

  val BaseConverter = new NumberConverter(0, "", NumberConverter.NULL_CONVERTER)
  val Thousands = new NumberConverter(3, "thousand", BaseConverter)
  val Millions = new NumberConverter(6, "million", Thousands)
  val Billions = new NumberConverter(9, "billion", Millions)
  val Trillions = new NumberConverter(12, "trillion", Billions)
  val Quadrillions = new NumberConverter(15, "quadrillion", Trillions)
  val Quintillions = new NumberConverter(18, "quintillion", Quadrillions)
  val Sextillions = new NumberConverter(21, "sextillion", Quintillions)
  val Septillions = new NumberConverter(24, "septillion", Sextillions)
  // If we're looking at numbers bigger than this, we have other problems.

  def apply(value: Long) = Septillions.convert(value.toString).trim
}

class NumberConverter(trailingDigits: Int, decorator: String, nextConverter: NumberConverter) {

  def convert(value: String): String = {
    (
      if (value.length > trailingDigits)
        convertHundreds( value.take(value.length - trailingDigits)) + s" $decorator "
      else
        ""
    ) + nextConverter.convert(value takeRight trailingDigits)
  }

  private val BAD_INPUT = "BAD INPUT!!"

  private def convertSingle(digit: String) = digit match {
    case "0" => ""
    case "1" => "one"
    case "2" => "two"
    case "3" => "three"
    case "4" => "four"
    case "5" => "five"
    case "6" => "six"
    case "7" => "seven"
    case "8" => "eight"
    case "9" => "nine"
    case _ => BAD_INPUT
  }

  private def convertTens(input: String): String = input match {
    case "00" => ""
    case "10" => "ten"
    case "11" => "eleven"
    case "12" => "twelve"
    case "13" => "thirteen"
    case "14" => "fourteen"
    case "15" => "fifteen"
    case "16" => "sixteen"
    case "17" => "seventeen"
    case "18" => "eighteen"
    case "19" => "nineteen"
    case "20" => "twenty"
    case "30" => "thirty"
    case "40" => "forty"
    case "50" => "fifty"
    case "60" => "sixty"
    case "70" => "seventy"
    case "80" => "eighty"
    case "90" => "ninety"
    case other => {
      if (other.length > 2) BAD_INPUT
      else if (other.length == 1) convertSingle(other)
      else if (other(0) == '0') convertSingle(other(1).toString)
      else convertTens(other(0) + "0") + " " + convertSingle(other(1).toString)
    }
  }

  private def convertHundreds(input: String): String = input match {
    case x if x.length > 3 => BAD_INPUT
    case x if x.length < 3 => convertTens(input.takeRight(2))
    case _ =>
      if (input(0) != '0')
        (convertSingle(input(0) toString) + " hundred " + convertTens(input.takeRight(2))).trim
      else
        convertTens(input.takeRight(2))
  }

}