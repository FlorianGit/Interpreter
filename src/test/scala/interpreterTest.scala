import org.scalatest.FlatSpec
import Interpreter._

class FirstSpec extends FlatSpec {
   "Numbers" should "be interpreted correctly" in {
      assert(interpret("3") === Number(3))
      assert(interpret("345") === Number(345))
      //assert(interpret("-35") === Number(-35)
   }

   "The char *" should "be interpreted to Times" in {
      assert(interpret("3*5") === Times(Number(3), Number(5)))
      assert(interpret("23*93") === Times(Number(23), Number(93)))
   }

   "The string 3*5" should "interpret . evaluate to 15" in {
      val text = "3*5"
      assert(interpret(text).evaluate === 15)
   }
}
