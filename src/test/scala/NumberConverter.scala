import org.scalatest.{Matchers, FlatSpec}

class NumberConverter extends FlatSpec with Matchers {

  "NumberConverter" should "convert a 4 into the string 'four'" in {
    NumberConverter(4) shouldBe "four"
  }

  it should "convert a 23 into the string 'twenty three'" in {
    NumberConverter(23) shouldBe "twenty three"
  }

  it should "convert a 10 to the string 'ten' without trailing space." in {
    NumberConverter(10) shouldBe "ten"
  }

  it should "convert a 20 to the string 'twenty'" in {
    NumberConverter(20) shouldBe "twenty"
  }

  it should "convert 123 to 'one hundred twenty three'" in {
    NumberConverter(123) shouldBe "one hundred twenty three"
  }

  it should "convert 42 to 'forty two'" in {
    NumberConverter(42) shouldBe "forty two"
  }

  it should "convert 300 into 'three hundred'" in {
    NumberConverter(300) shouldBe "three hundred"
  }

  it should "convert 501 to 'five hundred one'" in {
    NumberConverter(501) shouldBe "five hundred one"
  }

  it should "convert 123456 to 'one hundred twenty three thousand four hundred fifty six'" in {
    NumberConverter(123456) shouldBe "one hundred twenty three thousand four hundred fifty six"
  }

  it should "convert 1 234 567 890 987 654 321 to an appropriate english representation" in {
    NumberConverter(1234567890987654321L) shouldBe
      "one quintillion two hundred thirty four quadrillion five hundred sixty seven trillion eight hundred ninety billion nine hundred eighty seven million six hundred fifty four thousand three hundred twenty one"
  }


  object NumberConverter extends (Long => String) {
    val NULL_CONVERTER = new NumberConverter(0, null, null) {
      override def convert(x: String) = ""
    }

    private val BaseConverter = new NumberConverter(0, "", NumberConverter.NULL_CONVERTER)
    private val Thousands = new NumberConverter(3, "thousand", BaseConverter)
    private val Millions = new NumberConverter(6, "million", Thousands)
    private val Billions = new NumberConverter(9, "billion", Millions)
    private val Trillions = new NumberConverter(12, "trillion", Billions)
    private val Quadrillions = new NumberConverter(15, "quadrillion", Trillions)
    private val Quintillions = new NumberConverter(18, "quintillion", Quadrillions)
    private val Sextillions = new NumberConverter(21, "sextillion", Quintillions)
    private val Septillions = new NumberConverter(24, "septillion", Sextillions)
    // If we're looking at numbers bigger than this, we have other problems.

    def apply(value: Long) = Septillions.convert(value.toString).trim
  }

  private class NumberConverter(trailingDigits: Int, decorator: String, nextConverter: NumberConverter) {

    def convert(value: String): String = {
      (
        if (value.length > trailingDigits)
          convertHundreds( value.take(value.length - trailingDigits)) + s" $decorator "
        else
          ""
      ) + nextConverter.convert(value takeRight trailingDigits)
    }

    val BAD_INPUT = "BAD INPUT!!"

    def convertSingle(digit: String) = digit match {
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

    def convertTens(input: String): String = input match {
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

    def convertHundreds(input: String): String = input match {
      case x if x.length > 3 => BAD_INPUT
      case x if x.length < 3 => convertTens(input.takeRight(2))
      case _ =>
        if (input(0) != '0')
          (convertSingle(input(0) toString) + " hundred " + convertTens(input.takeRight(2))).trim
        else
          convertTens(input.takeRight(2))
    }

  }


  object Sections extends Enumeration {
    val ones, hundreds, thousands, millions, billions = Value
  }

  def convertHundreds = BaseConverter.convert _
  val input = "12345678"

  def func(input: String) = {
    val remainder = input.length % 3
    val trailingSections = (input.length - remainder) / 3
    for (section <- 1 to trailingSections + (if (remainder != 0) 1 else 0)) {
      convertHundreds(substring) + Sections(section)
    }
  }

}
