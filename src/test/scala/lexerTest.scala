import org.scalatest.FlatSpec
import Lexer._

class lexerSpec extends FlatSpec {
   "IntTokens" should "be interpreted correctly" in {
      var lexer = new Lexer("3")
      assert(lexer.Lex() === List(IntToken(3)))
      lexer = new Lexer("345")
      assert(lexer.Lex() === List(IntToken(345)))
      /*assert(val lexer = new Lexer("-35") === IntToken(-35))*/
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     assert (lexer.Lex() === List(IntToken(3), Plus(), IntToken(5), Plus(), IntToken(6)))
   }

   //"The char *" should "be interpreted to Times" in {
      //assert(val lexer = new Lexer("3*5") === Times(IntToken(3), IntToken(5)))
      //assert(val lexer = new Lexer("23*93") === Times(IntToken(23), IntToken(93)))
      //assert(interpret2("3*5") === Times(IntToken(3), IntToken(5)))
      //assert(interpret2("23*93") === Times(IntToken(23), IntToken(93)))
   //}

   //"Times" should "have higher priority than Plus" in {
      //assert(val lexer = new Lexer("2+3*5") === Plus(IntToken(2), Times(IntToken(3), IntToken(5))))
      //assert(val lexer = new Lexer("1*3+6") === Plus(Times(IntToken(1), IntToken(3)), IntToken(6)))
      //assert(interpret2("2+3*5") === Plus(IntToken(2), Times(IntToken(3), IntToken(5))))
      //assert(interpret2("1*3+6") === Plus(Times(IntToken(1), IntToken(3)), IntToken(6)))
   //}

   "Minus" should "be interpreted correctly" in {
     
      var lexer = new Lexer("5-3")
      assert(lexer.Lex() === List(IntToken(5), Minus(), IntToken(3)))
      lexer = new Lexer("5-6-4")
      assert(lexer.Lex() === List(IntToken(5), Minus(), IntToken(6), Minus(), IntToken(4)))
   }

   "Parentheses" should "be handled correctly" in {
     var lexer = new Lexer("(3+5)+(2-4)")
     assert(lexer.Lex() === List(ParenthesisOpen(), IntToken(3), Plus(), IntToken(5), ParenthesisClose(), Plus(), ParenthesisOpen(), IntToken(2), Minus(), IntToken(4), ParenthesisClose()))

     lexer = new Lexer("7 + 3 * (10 / (12 / (3 + 1) - 1))")
     assert(lexer.Lex() === List(IntToken(7),Plus(),IntToken(3),Times(),ParenthesisOpen(), IntToken(10), Div(), ParenthesisOpen(), IntToken(12), Div(), ParenthesisOpen(), IntToken(3), Plus(), IntToken(1), ParenthesisClose(), Minus(), IntToken(1), ParenthesisClose(), ParenthesisClose()))
   }


   //"The string 3*5" should "interpret . evaluate to 15" in {
      //val text = "3*5"
      //assert(val lexer = new Lexer(text).evaluate === 15)
      //assert(interpret2(text).evaluate === 15)
   //}
}
