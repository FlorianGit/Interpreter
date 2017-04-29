import org.scalatest.FlatSpec
import Calculator._

class FirstSpec extends FlatSpec {
   "Numbers" should "be interpreted correctly" in {
      assert(interpret("3") === Number(3))
      assert(interpret("345") === Number(345))
      assert(interpret2("3") === Number(3))
      assert(interpret2("345") === Number(345))
      /*assert(interpret("-35") === Number(-35))*/
   }

   "The char *" should "be interpreted to Times" in {
      assert(interpret("3*5") === Times(Number(3), Number(5)))
      assert(interpret("23*93") === Times(Number(23), Number(93)))
      assert(interpret2("3*5") === Times(Number(3), Number(5)))
      assert(interpret2("23*93") === Times(Number(23), Number(93)))
   }

   "Times" should "have higher priority than Plus" in {
      assert(interpret("2+3*5") === Plus(Number(2), Times(Number(3), Number(5))))
      assert(interpret("1*3+6") === Plus(Times(Number(1), Number(3)), Number(6)))
      assert(interpret2("2+3*5") === Plus(Number(2), Times(Number(3), Number(5))))
      assert(interpret2("1*3+6") === Plus(Times(Number(1), Number(3)), Number(6)))
   }

   "Minus" should "be interpreted correctly" in {
      assert(interpret("5-3") === Minus(Number(5), Number(3)))
      assert(interpret("5-6-4") === Minus(Minus(Number(5), Number(6)), Number(4)))
      assert(interpret2("5-3") === Minus(Number(5), Number(3)))
      assert(interpret2("5-6-4") === Minus(Minus(Number(5), Number(6)), Number(4)))
   }

   "The string 3*5" should "interpret . evaluate to 15" in {
      val text = "3*5"
      assert(interpret(text).evaluate === 15)
      assert(interpret2(text).evaluate === 15)
   }
}
