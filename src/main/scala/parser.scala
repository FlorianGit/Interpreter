import scala.collection.mutable.ArrayBuffer
import reflect.ClassTag
import Lexer._

package Parser {
  trait AST
  case class BinOp(op: Token, left: AST, right: AST) extends AST
  case class Number(n: Token) extends AST
  case class Assign(id: Token, rhs: AST) extends AST
  case class Var(id: Token) extends AST
  case class StatementList(statements: ArrayBuffer[AST]) extends AST
  case class NoOp() extends AST

  class Parser(lexer: Lexer) {
    var currentToken = lexer.getNextToken()

    def eat[T](implicit T: ClassTag[T]) =
      if (T.runtimeClass == currentToken.getClass) currentToken = lexer.getNextToken() else throw new Exception(T.runtimeClass.toString ++ " Expected. Received:" ++ currentToken.getClass.toString)

    def factor(): AST = currentToken match {
      case ParenthesisOpen() => {
        eat[ParenthesisOpen]
        val ret = expr()
        eat[ParenthesisClose]
        ret
      }
      case IntConst(_) => {
          val token = currentToken
          eat[IntConst]
          new Number(token)
        }
      case _  => {
          val token = currentToken
          eat[Id]
          new Var(token)
        }
    }

    def mult(): AST = {
      def isMultIntDiv(t: Token) = t match {
        case Times() | IntDiv() => true
        case _ => false
      }

      var result = factor()
      while (isMultIntDiv(currentToken)) {
        currentToken match {
          case Times() => {
            eat[Times]
            result = new BinOp(new Times(), result, factor())
          }
          case IntDiv() => {
            eat[IntDiv]
            result = new BinOp(new IntDiv(), result, factor())
          }
        }
      }
      result
    }

    def expr(): AST = {
      def isPlusMinus(t: Token) = t match {
        case Plus() | Minus() => true
        case _ => false
      }

      var result = mult()
      while (isPlusMinus(currentToken)) {
        currentToken match {
          case Plus() => {
            eat[Plus]
            result = new BinOp(new Plus(), result, mult())
          }
          case Minus() => {
            eat[Minus]
            result = new BinOp(new Minus(), result, mult())
          }
        }
      }
      result
    }

    def assignment_statement(): AST = {
        val varToken = currentToken
        eat[Id]
        eat[AssignToken]
        new Assign(varToken, expr())
    }

    def statement(): AST = currentToken match {
      case Begin() => compound_statement()
      case Id(_) => assignment_statement()
      case _ => new NoOp()
    }

    def statement_list(): AST = {
      val childStatements = ArrayBuffer(statement())
      while (currentToken == SemiColon()) {
        eat[SemiColon]
        childStatements += statement()
      }
      new StatementList(childStatements)
    }

    def compound_statement(): AST = {
      eat[Begin]
      val result = statement_list()
      eat[End]
      result
    }

    def program(): AST = {
      val result = compound_statement()
      eat[Dot]
      result
    }

  }
}
