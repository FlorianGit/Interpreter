import org.scalatest.FlatSpec
import scala.collection.mutable.ArrayBuffer
import Lexer._
import Parser._
import Interpreter._

class parserSpec extends FlatSpec {
   "IntTokens" should "be interpreted correctly" in {
     var parser = new Parser(new Lexer("3"))
     assert(parser.expr() === Number(IntToken(3)))
     //parser = new Parser(new Lexer("3"))
     //var interpreter = new Interpreter()
     //assert(interpreter.evaluate(parser.expr()) === 3)

     parser = new Parser(new Lexer("345"))
     assert(parser.expr() === Number(IntToken(345)))
     //interpreter = new Interpreter()
     //parser = new Parser(new Lexer("345"))
     //assert(interpreter.evaluate(parser.expr()) === 345) 
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Plus(), BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5))), Number(IntToken(6))))
     //var interpreter = new Interpreter()
     //lexer = new Lexer("3+5+6")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 14) 
   }

   "Minus" should "be interpreted correctly" in {
      var lexer = new Lexer("5-3")
      var parser = new Parser(lexer)
      assert(parser.expr() === BinOp(Minus(), Number(IntToken(5)), Number(IntToken(3))))
     //var interpreter = new Interpreter()
      //lexer = new Lexer("5-3")
      //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 2) 

      lexer = new Lexer("5-6-4")
      parser = new Parser(lexer)
      assert(parser.expr() === BinOp(Minus(), BinOp(Minus(), Number(IntToken(5)), Number(IntToken(6))), Number(IntToken(4))))
     //interpreter = new Interpreter()
      //lexer = new Lexer("5-6-4")
      //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === -5) 
   }

   "Multiplication" should "work correctly" in {
     var lexer = new Lexer("3*5")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Times(), Number(IntToken(3)), Number(IntToken(5))))
     //var interpreter = new Interpreter()
     //lexer = new Lexer("3*5")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 15) 

     lexer = new Lexer("3+6*5")
     parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Plus(), Number(IntToken(3)), BinOp(Times(), Number(IntToken(6)), Number(IntToken(5)))))
     //interpreter = new Interpreter()
     //lexer = new Lexer("3+6*5")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 33) 
   }

   "Parentheses" should "go before all other operations" in {
     var lexer = new Lexer("(3+5)*2")
     var parser = new Parser(lexer)
     assert(parser.expr() === BinOp(Times(), BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5))), Number(IntToken(2))))
     //var interpreter = new Interpreter()
     //lexer = new Lexer("(3+5)*2")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 16) 

     lexer = new Lexer("(3+5)*((2-1)+(4+3))")
     parser = new Parser(lexer)
     val threeplusfive = BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5)))
     val twominusone = BinOp(Minus(), Number(IntToken(2)), Number(IntToken(1)))
     val fourplusthree = BinOp(Plus(), Number(IntToken(4)), Number(IntToken(3)))
     val rightside = BinOp(Plus(), twominusone, fourplusthree)
     assert(parser.expr() === BinOp(Times(), threeplusfive, rightside))
     //interpreter = new Interpreter()
     //lexer = new Lexer("(3+5)*((2-1)+(4+3))")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 64) 

     lexer = new Lexer("(12/(3+1))")
     parser = new Parser(lexer)
     val threeplusone = BinOp(Plus(), Number(IntToken(3)), Number(IntToken(1)))
     val total = BinOp(Div(), Number(IntToken(12)), threeplusone)
     assert(parser.expr() === total)
     //interpreter = new Interpreter()
     //lexer = new Lexer("(12/(3+1))")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 3) 

     lexer = new Lexer("12/(3+1)-1")
     parser = new Parser(lexer)
     val total2 =  BinOp(Minus(), total, Number(IntToken(1)))
     assert(parser.expr() === total2)
     //interpreter = new Interpreter()
     //lexer = new Lexer("12/(3+1)-1")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 2) 

     lexer = new Lexer("10/(12/(3+1)-1)")
     parser = new Parser(lexer)
     val total3 = BinOp(Div(), Number(IntToken(10)), total2)
     assert(parser.expr() === total3)
     //interpreter = new Interpreter()
     //lexer = new Lexer("10/(12/(3+1)-1)")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 5) 

     lexer = new Lexer("7+3*(10/(12/(3+1)-1))")
     parser = new Parser(lexer)
     val total4 = BinOp(Times(), Number(IntToken(3)), total3)
     assert(parser.expr() === BinOp(Plus(), Number(IntToken(7)), total4))
     //interpreter = new Interpreter()
     //lexer = new Lexer("7+3*(10/(12/(3+1)-1))")
     //parser = new Parser(lexer)
     //assert(interpreter.evaluate(parser.expr()) === 22) 
   }

   "Assigment" should "work" in {
     var lexer = new Lexer("x:=5")
     var parser = new Parser(lexer)
     assert(parser.assignment_statement() === Assign(Id("x"), Number(IntToken(5))))

     lexer = new Lexer("variable:= (3 + 5) * y")
     parser = new Parser(lexer)
     assert(parser.assignment_statement() === Assign(Id("variable"), BinOp(Times(), BinOp(Plus(), Number(IntToken(3)), Number(IntToken(5))), Var(Id("y")))))
   }

   "A small program" should "be parsed correctly" in {
     val smallProgram = """
     BEGIN
        BEGIN
           number := 2;
           a := number;
           b := 10 * a + 10 * number / 4;
           c := a - b;
        END;
        x := 10
     END.
     """
     def st1 = Assign(Id("number"), Number(IntToken(2)))
     def st2 = Assign(Id("a"), Var(Id("number")))
     def st3 = Assign(Id("b"), BinOp(Plus(), BinOp(Times(), Number(IntToken(10)), Var(Id("a"))), BinOp(Div(), BinOp(Times(), Number(IntToken(10)), Var(Id("number"))), Number((IntToken(4))))))
     def st4 = Assign(Id("c"), BinOp(Minus(), Var(Id("a")), Var(Id("b"))))
     def st5 = Assign(Id("x"), Number(IntToken(10)))
     def lexer = new Lexer(smallProgram)
     def parser = new Parser(lexer)
     assert(parser.program() === StatementList(ArrayBuffer(StatementList(ArrayBuffer(st1, st2, st3, st4, NoOp())), st5)))
   }

}
