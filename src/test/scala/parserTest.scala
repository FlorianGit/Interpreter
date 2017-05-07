import org.scalatest.FlatSpec
import Lexer._

class parserSpec extends FlatSpec {
   "Numbers" should "be interpreted correctly" in {
     var parser = new Parser(new Lexer("3"))
     assert(parser.expr() === Numerical(Number(3)))

     parser = new Parser(new Lexer("345"))
     assert(parser.expr() === Numerical(Number(345)))
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Plus(), BinOp(Plus(), Numerical(Number(3)), Numerical(Number(5))), Numerical(Number(6))))
   }

   "Minus" should "be interpreted correctly" in {
     
      var lexer = new Lexer("5-3")
      var parser = new Parser(lexer)
      assert(parser.expr() === BinOp(Minus(), Numerical(Number(5)), Numerical(Number(3))))

      lexer = new Lexer("5-6-4")
      parser = new Parser(lexer)
      assert(parser.expr() === BinOp(Minus(), BinOp(Minus(), Numerical(Number(5)), Numerical(Number(6))), Numerical(Number(4))))
   }

   "Multiplication" should "work correctly" in {
     var lexer = new Lexer("3*5")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Times(), Numerical(Number(3)), Numerical(Number(5))))

     lexer = new Lexer("3+6*5")
     parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Plus(), Numerical(Number(3)), BinOp(Times(), Numerical(Number(6)), Numerical(Number(5)))))
   }

   "Parentheses" should "go before all other operations" in {
     var lexer = new Lexer("(3+5)*2")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Times(), BinOp(Plus(), Numerical(Number(3)), Numerical(Number(5))), Numerical(Number(2))))

     lexer = new Lexer("(3+5)*((2-1)+(4+3))")
     parser = new Parser(lexer)
     val threeplusfive = BinOp(Plus(), Numerical(Number(3)), Numerical(Number(5)))
     val twominusone = BinOp(Minus(), Numerical(Number(2)), Numerical(Number(1)))
     val fourplusthree = BinOp(Plus(), Numerical(Number(4)), Numerical(Number(3)))
     val rightside = BinOp(Plus(), twominusone, fourplusthree)
     assert(parser.expr() === BinOp(Times(), threeplusfive, rightside))

     lexer = new Lexer("(12/(3+1))")
     parser = new Parser(lexer)
     val threeplusone = BinOp(Plus(), Numerical(Number(3)), Numerical(Number(1)))
     val total = BinOp(Div(), Numerical(Number(12)), threeplusone)
     assert(parser.expr() === total)

     lexer = new Lexer("12/(3+1)-1")
     parser = new Parser(lexer)
     val total2 =  BinOp(Minus(), total, Numerical(Number(1)))
     assert(parser.expr() === total2)

     lexer = new Lexer("10/(12/(3+1)-1)")
     parser = new Parser(lexer)
     val total3 = BinOp(Div(), Numerical(Number(10)), total2)
     assert(parser.expr() === total3)

     lexer = new Lexer("7+3*(10/(12/(3+1)-1))")
     parser = new Parser(lexer)
     val total4 = BinOp(Times(), Numerical(Number(3)), total3)
     assert(parser.expr() === BinOp(Plus(), Numerical(Number(7)), total4))
   }
}
