import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

// FizzBuzz implemented with the rules abstracted away.
class FizzBuzz extends FlatSpec with ShouldMatchers{

  type GameRule = PartialFunction[Int, String]

  val Fizz: GameRule = {case x if x % 3 == 0 => "Fizz"}
  val Buzz: GameRule = {case x if x % 5 == 0 => "Buzz"}
  val Whizz: GameRule = {case x if x.toString endsWith "3" => "Whizz"}
  val Bazz: GameRule = {case x if x % 7 == 0 => "Bazz"}

  class GameRunner(rules: GameRule*) {

    def running(value: Int) = {
      val appliedRules = rules filter ( _ isDefinedAt value ) map ( _ apply value )
      appliedRules match {
        case Nil => value.toString
        case x => x.mkString(" ")
      }
    }
  }

  "GameRunner" should "return 2 when given a 2 and an empty rule set." in {
    new GameRunner() running 2 should be("2")
  }

  it should "return a 3 when not given a fizz rule." in {
    new GameRunner() running 3 should be("3")
  }

  it should "return a 5 when given a 5 and not a buzz rule." in {
    new GameRunner() running 5 should be("5")
  }
  
  it should "return a Buzz when given a 5 and a buzz rule." in {
    new GameRunner(Buzz) running 5 should be("Buzz")
  }

  it should "return a Fizz when given a 3 and a fizz rule." in {
    new GameRunner(Fizz) running 3 should be("Fizz")
  }

  it should "return 'Fizz Buzz' when given a 15 and both a fizz and buzz rule." in {
    new GameRunner(Fizz, Buzz) running 15 should be ("Fizz Buzz")
  }

  it should "return 'Buzz Fizz' when given a 15 and a buzz and fizz rule, with the buzz rule first." in {
    new GameRunner(Buzz, Fizz) running 15 should be ("Buzz Fizz")
  }

  it should "return a Whizz when given a 13 and a whizz rule." in {
    new GameRunner(Whizz) running 13 should be ("Whizz")
  }

  it should "return a 32 when given a 32 and a whizz rule." in {
    new GameRunner(Whizz) running 32 should be ("32")
  }

  it should "return a Bazz when given a 14 and a bazz rule." in {
    new GameRunner(Bazz) running 14 should be ("Bazz")
  }

  "Fizz Buzz" should " have run correctly." in {
    val runner = new GameRunner(Fizz, Buzz, Bazz, Whizz)
    1 to 100 map runner.running foreach println
  }

}
