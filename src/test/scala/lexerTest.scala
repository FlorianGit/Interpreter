import org.scalatest.FlatSpec
import Lexer._

class lexerSpec extends FlatSpec {
   "Numbers" should "be interpreted correctly" in {
      var lexer = new Lexer("3")
      assert(lexer.Lex() === List(Number(3)))
      lexer = new Lexer("345")
      assert(lexer.Lex() === List(Number(345)))
      /*assert(val lexer = new Lexer("-35") === Number(-35))*/
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     assert (lexer.Lex() === List(Number(3), Plus(), Number(5), Plus(), Number(6)))
   }

   //"The char *" should "be interpreted to Times" in {
      //assert(val lexer = new Lexer("3*5") === Times(Number(3), Number(5)))
      //assert(val lexer = new Lexer("23*93") === Times(Number(23), Number(93)))
      //assert(interpret2("3*5") === Times(Number(3), Number(5)))
      //assert(interpret2("23*93") === Times(Number(23), Number(93)))
   //}

   //"Times" should "have higher priority than Plus" in {
      //assert(val lexer = new Lexer("2+3*5") === Plus(Number(2), Times(Number(3), Number(5))))
      //assert(val lexer = new Lexer("1*3+6") === Plus(Times(Number(1), Number(3)), Number(6)))
      //assert(interpret2("2+3*5") === Plus(Number(2), Times(Number(3), Number(5))))
      //assert(interpret2("1*3+6") === Plus(Times(Number(1), Number(3)), Number(6)))
   //}

   "Minus" should "be interpreted correctly" in {
     
      var lexer = new Lexer("5-3")
      assert(lexer.Lex() === List(Number(5), Minus(), Number(3)))
      lexer = new Lexer("5-6-4")
      assert(lexer.Lex() === List(Number(5), Minus(), Number(6), Minus(), Number(4)))
   }

   "Parentheses" should "be handled correctly" in {
     var lexer = new Lexer("(3+5)+(2-4)")
     assert(lexer.Lex() === List(ParenthesisOpen(), Number(3), Plus(), Number(5), ParenthesisClose(), Plus(), ParenthesisOpen(), Number(2), Minus(), Number(4), ParenthesisClose()))

     lexer = new Lexer("7+3*(10/(12/(3+1)-1))")
     assert(lexer.Lex() === List(Number(7),Plus(),Number(3),Times(),ParenthesisOpen(), Number(10), Div(), ParenthesisOpen(), Number(12), Div(), ParenthesisOpen(), Number(3), Plus(), Number(1), ParenthesisClose(), Minus(), Number(1), ParenthesisClose(), ParenthesisClose()))
   }


   //"The string 3*5" should "interpret . evaluate to 15" in {
      //val text = "3*5"
      //assert(val lexer = new Lexer(text).evaluate === 15)
      //assert(interpret2(text).evaluate === 15)
   //}
}
