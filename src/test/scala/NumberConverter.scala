import org.scalatest.{Matchers, FlatSpec}

class NumberConverterTest extends FlatSpec with Matchers {

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
      "one quintillion " +
        "two hundred thirty four quadrillion " +
        "five hundred sixty seven trillion " +
        "eight hundred ninety billion " +
        "nine hundred eighty seven million " +
        "six hundred fifty four thousand " +
        "three hundred twenty one"
  }
}







