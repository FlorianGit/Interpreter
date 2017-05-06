import org.scalatest.FlatSpec
import Lexer._

class parserSpec extends FlatSpec {
   "Numbers" should "be interpreted correctly" in {
     var parser = new Parser(new Lexer("3"))
     assert(parser.expr() === 3)

     parser = new Parser(new Lexer("345"))
     assert(parser.expr() === 345)
      /*assert(val lexer = new Lexer("-35") === Number(-35))*/
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     var parser = new Parser(lexer)
     //assert (parse(lexer) === List(Number(3), Plus(), Number(5), Plus(), Number(6)))
     assert(parser.expr() === 14)
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
      var parser = new Parser(lexer)
      //assert(lexer.Lex() === List(Number(5), Minus(), Number(3)))
      //assert(parse(lexer) === PMinus(PNumber(5), PNumber(3)))
      assert(parser.expr() === 2)

      lexer = new Lexer("5-6-4")
      parser = new Parser(lexer)
      assert(parser.expr() === -5)
      //assert(lexer.Lex() === List(Number(5), Minus(), Number(6), Minus(), Number(4)))
      //assert(parse(lexer)) === PMinus(PMinus(PNumber(5), PNumber(6)), PNumber(4)))
   }

   //"The string 3*5" should "interpret . evaluate to 15" in {
      //val text = "3*5"
      //assert(val lexer = new Lexer(text).evaluate === 15)
      //assert(interpret2(text).evaluate === 15)
   //}
   "Multiplication" should "work correctly" in {
     var lexer = new Lexer("3*5")
     var parser = new Parser(lexer)
     assert(parser.expr() === 15)

     lexer = new Lexer("3+6*5")
     parser = new Parser(lexer)
     assert(parser.expr() === 33)
   }

   "Parentheses" should "go before all other operations" in {
     var lexer = new Lexer("(3+5)*2")
     var parser = new Parser(lexer)
     assert(parser.expr() === 16)

     lexer = new Lexer("(3+5)*((2-1)+(4+3))")
     parser = new Parser(lexer)
     assert(parser.expr() === 64)

     lexer = new Lexer("(12/(3+1))")
     parser = new Parser(lexer)
     assert(parser.expr() == 3)

     lexer = new Lexer("12/(3+1)-1")
     parser = new Parser(lexer)
     assert(parser.expr() === 2)

     lexer = new Lexer("10/(12/(3+1)-1)")
     parser = new Parser(lexer)
     assert(parser.expr() === 5)

     lexer = new Lexer("7+3*(10/(12/(3+1)-1))")
     parser = new Parser(lexer)
     assert(parser.expr() === 22)
   }
}
