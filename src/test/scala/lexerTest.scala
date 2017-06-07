import org.scalatest.FlatSpec
import Lexer._

class lexerSpec extends FlatSpec {
   "IntConsts" should "be interpreted correctly" in {
      var lexer = new Lexer("3")
      assert(lexer.Lex() === List(IntConst(3)))
      lexer = new Lexer("345")
      assert(lexer.Lex() === List(IntConst(345)))
      /*assert(val lexer = new Lexer("-35") === IntConst(-35))*/
   }

   "FloatConsts" should "be lexed correctly" in {
     var lexer = new Lexer("3.5")
     var value = lexer.Lex() match {case FloatConst(n):: Nil => n; case _ => 0}
     assert(value - 3.5 < 0.01)
     lexer = new Lexer("0.4")
     value = lexer.Lex() match {case FloatConst(n):: Nil => n; case _ => 0}
     assert(value - 3.5 < 0.01)
   }

   "Plus" should "be interpreted correctly" in {
     var lexer = new Lexer("3+5+6")
     assert (lexer.Lex() === List(IntConst(3), Plus(), IntConst(5), Plus(), IntConst(6)))
   }

   "Minus" should "be interpreted correctly" in {
      var lexer = new Lexer("5-3")
      assert(lexer.Lex() === List(IntConst(5), Minus(), IntConst(3)))
      lexer = new Lexer("5-6-4")
      assert(lexer.Lex() === List(IntConst(5), Minus(), IntConst(6), Minus(), IntConst(4)))
   }

   "Parentheses" should "be handled correctly" in {
     var lexer = new Lexer("(3+5)+(2-4)")
     assert(lexer.Lex() === List(ParenthesisOpen(), IntConst(3), Plus(), IntConst(5), ParenthesisClose(), Plus(), ParenthesisOpen(), IntConst(2), Minus(), IntConst(4), ParenthesisClose()))

     lexer = new Lexer("7 + 3 * (10 div  (12 div (3 + 1) - 1))")
     assert(lexer.Lex() === List(IntConst(7),Plus(),IntConst(3),Times(),ParenthesisOpen(), IntConst(10), IntDiv(), ParenthesisOpen(), IntConst(12), IntDiv(), ParenthesisOpen(), IntConst(3), Plus(), IntConst(1), ParenthesisClose(), Minus(), IntConst(1), ParenthesisClose(), ParenthesisClose()))
   }

   "Assignment" should "work correctly" in {
     var lexer = new Lexer("x:=5")
     assert(lexer.Lex() === List(Id("x"), AssignToken(), IntConst(5)))
     lexer = new Lexer("variable := 12")
     assert(lexer.Lex() === List(Id("variable"), AssignToken(), IntConst(12)))

     lexer = new Lexer("variable:= (3 + 5) * y")
     assert(lexer.Lex() === List(Id("variable"), AssignToken(), ParenthesisOpen(), IntConst(3), Plus(), IntConst(5), ParenthesisClose(), Times(), Id("y")))
   }

   "A small program" should "be lexed correctly" in {
     val smallProgram = """
     PROGRAM testprogram;
     VAR
     testint: INTEGER;
     testfloat1, testfloat2: FLOAT
     BEGIN
        BEGIN
           number := 2;
           a := number;
           b := 10 * a + 10 * number div 4;
           c := a - b;
        END;
        x := 10
     END.
     """
     def decl = List(Program(), Id("testprogram"), SemiColon(), VarKeyword(), Id("testint"), Colon(), Integer(), SemiColon(), Id("testfloat1"), Comma(), Id("testfloat2"),  Colon(), FloatKeyword())
     def st1 = List(Id("number"), AssignToken(), IntConst(2), SemiColon())
     def st2 = List(Id("a"), AssignToken(), Id("number"), SemiColon())
     def st3 = List(Id("b"), AssignToken(), IntConst(10), Times(), Id("a"), Plus(), IntConst(10), Times(), Id("number"), IntDiv(), IntConst(4), SemiColon())
     def st4 = List(Id("c"), AssignToken(), Id("a"), Minus(), Id("b"), SemiColon())
     def st5 = List(Id("x"), AssignToken(), IntConst(10))
     def lexer = new Lexer(smallProgram)
     assert(lexer.Lex() === decl ++ List(Begin(), Begin()) ++ st1 ++ st2 ++ st3 ++ st4 ++ List(End(), SemiColon()) ++ st5 ++ List(End(), Dot(), EOF()))
   }

}
