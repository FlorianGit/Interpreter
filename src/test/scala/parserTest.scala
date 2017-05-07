import org.scalatest.FlatSpec
import Lexer._
import Parser._

class parserSpec extends FlatSpec {
   "IntTokens" should "be interpreted correctly" in {
     var parser = new Parser(new Lexer("3"))
     assert(parser.expr() === Number(IntToken(3)))

     parser = new Parser(new Lexer("345"))
     assert(parser.expr() === Number(IntToken(345)))
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Plus(), BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5))), Number(IntToken(6))))
   }

   "Minus" should "be interpreted correctly" in {
     
      var lexer = new Lexer("5-3")
      var parser = new Parser(lexer)
      assert(parser.expr() === BinOp(Minus(), Number(IntToken(5)), Number(IntToken(3))))

      lexer = new Lexer("5-6-4")
      parser = new Parser(lexer)
      assert(parser.expr() === BinOp(Minus(), BinOp(Minus(), Number(IntToken(5)), Number(IntToken(6))), Number(IntToken(4))))
   }

   "Multiplication" should "work correctly" in {
     var lexer = new Lexer("3*5")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Times(), Number(IntToken(3)), Number(IntToken(5))))

     lexer = new Lexer("3+6*5")
     parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Plus(), Number(IntToken(3)), BinOp(Times(), Number(IntToken(6)), Number(IntToken(5)))))
   }

   "Parentheses" should "go before all other operations" in {
     var lexer = new Lexer("(3+5)*2")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Times(), BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5))), Number(IntToken(2))))

     lexer = new Lexer("(3+5)*((2-1)+(4+3))")
     parser = new Parser(lexer)
     val threeplusfive = BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5)))
     val twominusone = BinOp(Minus(), Number(IntToken(2)), Number(IntToken(1)))
     val fourplusthree = BinOp(Plus(), Number(IntToken(4)), Number(IntToken(3)))
     val rightside = BinOp(Plus(), twominusone, fourplusthree)
     assert(parser.expr() === BinOp(Times(), threeplusfive, rightside))

     lexer = new Lexer("(12/(3+1))")
     parser = new Parser(lexer)
     val threeplusone = BinOp(Plus(), Number(IntToken(3)), Number(IntToken(1)))
     val total = BinOp(Div(), Number(IntToken(12)), threeplusone)
     assert(parser.expr() === total)

     lexer = new Lexer("12/(3+1)-1")
     parser = new Parser(lexer)
     val total2 =  BinOp(Minus(), total, Number(IntToken(1)))
     assert(parser.expr() === total2)

     lexer = new Lexer("10/(12/(3+1)-1)")
     parser = new Parser(lexer)
     val total3 = BinOp(Div(), Number(IntToken(10)), total2)
     assert(parser.expr() === total3)

     lexer = new Lexer("7+3*(10/(12/(3+1)-1))")
     parser = new Parser(lexer)
     val total4 = BinOp(Times(), Number(IntToken(3)), total3)
     assert(parser.expr() === BinOp(Plus(), Number(IntToken(7)), total4))
   }
}
