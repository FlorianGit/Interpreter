import scala.collection.mutable.ArrayBuffer
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

    def eat(tokenType: String) = currentToken match {
      case Plus() => if (tokenType == "PLUS") currentToken = lexer.getNextToken() else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case Minus() => if (tokenType == "MINUS") currentToken = lexer.getNextToken() else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case Times() => if (tokenType == "TIMES") currentToken = lexer.getNextToken() else throw new Exception(tokenType ++ "expteced, " ++ currentToken.toString)
      case Div() => if (tokenType == "DIV") currentToken = lexer.getNextToken() else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case ParenthesisOpen() => if (tokenType == "PARENTHOPEN") currentToken = lexer.getNextToken() else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case ParenthesisClose() => if (tokenType == "PARENTHCLOSE") currentToken = lexer.getNextToken() else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case Id(_) => if (tokenType == "ID") currentToken = lexer.getNextToken else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case AssignToken() => if (tokenType == "ASSIGN") currentToken = lexer.getNextToken else throw new Exception(tokenType ++ " expected, " ++ currentToken.toString)
      case _ => currentToken = lexer.getNextToken()
    }

    def factor(): AST = currentToken match {
      case ParenthesisOpen() => {
        eat("PARENTHOPEN")
        val ret = expr()
        eat("PARENTHCLOSE")
        ret
      }
      case IntToken(_) => {
          val token = currentToken
          eat("INTEGER")
          new Number(token)
        }
      case _  => {
          val token = currentToken
          eat("ID")
          new Var(token)
        }
    }

    def mult(): AST = {
      def isMultDiv(t: Token) = t match {
        case Times() | Div() => true
        case _ => false
      }

      var result = factor()
      while (isMultDiv(currentToken)) {
        currentToken match {
          case Times() => {
            eat("TIMES")
            result = new BinOp(new Times(), result, factor())
          }
          case Div() => {
            eat("DIV")
            result = new BinOp(new Div(), result, factor())
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
            eat("PLUS")
            result = new BinOp(new Plus(), result, mult())
          }
          case Minus() => {
            eat("MINUS")
            result = new BinOp(new Minus(), result, mult())
          }
        }
      }
      result
    }

    def assignment_statement(): AST = {
        val varToken = currentToken
        eat("ID")
        eat("ASSIGN")
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
        eat("SEMICOLON")
        childStatements += statement()
      }
      new StatementList(childStatements)
    }

    def compound_statement(): AST = {
      eat("BEGIN")
      val result = statement_list()
      eat("END")
      result
    }

    def program(): AST = {
      val result = compound_statement()
      eat("DOT")
      result
    }

  }
}
