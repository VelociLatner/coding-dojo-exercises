import org.scalatest.{Matchers, FlatSpec}

class RyansConverterTest extends FlatSpec with Matchers {
  val RC = new RyansConverter

  "Ryan's Converter" should "convert a 4 into the string 'four'" in {
    RC(4) shouldBe "four"
  }

  it should "convert a 23 into the string 'twenty three'" in {
    RC(23) shouldBe "twenty three"
  }

  it should "convert a 10 to the string 'ten' without trailing space." in {
    RC(10) shouldBe "ten"
  }

  it should "convert a 20 to the string 'twenty'" in {
    RC(20) shouldBe "twenty"
  }

  it should "convert 123 to 'one hundred twenty three'" in {
    RC(123) shouldBe "one hundred twenty three"
  }

  it should "convert 42 to 'forty two'" in {
    RC(42) shouldBe "forty two"
  }

  it should "convert 300 into 'three hundred'" in {
    RC(300) shouldBe "three hundred"
  }

  it should "convert 501 to 'five hundred one'" in {
    RC(501) shouldBe "five hundred one"
  }

  it should "convert 123456 to 'one hundred twenty three thousand four hundred fifty six'" in {
    RC(123456) shouldBe "one hundred twenty three thousand four hundred fifty six"
  }


}
