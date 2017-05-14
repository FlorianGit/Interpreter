import org.scalatest.FlatSpec
import Lexer._
import Parser._
import Interpreter._

class InterpreterSpec extends FlatSpec {
  "A small program" should "lead to the right varTable" in {

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
    val lexer = new Lexer(smallProgram)
    val parser = new Parser(lexer)
    val interpreter = new Interpreter(parser)
    assert(interpreter.printVarTable() === "Map(b -> 25, number -> 2, a -> 2, x -> 10, c -> -23)")
  }
}
