import scala.collection.mutable.ArrayBuffer
import reflect.runtime.universe.{TypeTag, typeTag}
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

    def eat[T](tokenType: TypeTag[T]) = currentToken match {
      case Plus() => if (tokenType == typeTag[Plus]) currentToken = lexer.getNextToken() else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case Minus() => if (tokenType == typeTag[Minus]) currentToken = lexer.getNextToken() else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case Times() => if (tokenType == typeTag[Times]) currentToken = lexer.getNextToken() else throw new Exception(tokenType.toString ++ "expteced, " ++ currentToken.toString)
      case Div() => if (tokenType == typeTag[Div]) currentToken = lexer.getNextToken() else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case ParenthesisOpen() => if (tokenType == typeTag[ParenthesisOpen]) currentToken = lexer.getNextToken() else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case ParenthesisClose() => if (tokenType == typeTag[ParenthesisClose]) currentToken = lexer.getNextToken() else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case Id(_) => if (tokenType == typeTag[Id]) currentToken = lexer.getNextToken else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case AssignToken() => if (tokenType == typeTag[AssignToken]) currentToken = lexer.getNextToken else throw new Exception(tokenType.toString ++ " expected, " ++ currentToken.toString)
      case _ => currentToken = lexer.getNextToken()
    }

    def factor(): AST = currentToken match {
      case ParenthesisOpen() => {
        eat(typeTag[ParenthesisOpen])
        val ret = expr()
        eat(typeTag[ParenthesisClose])
        ret
      }
      case IntToken(_) => {
          val token = currentToken
          eat(typeTag[IntToken])
          new Number(token)
        }
      case _  => {
          val token = currentToken
          eat(typeTag[Id])
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
            eat(typeTag[Times])
            result = new BinOp(new Times(), result, factor())
          }
          case Div() => {
            eat(typeTag[Div])
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
            eat(typeTag[Plus])
            result = new BinOp(new Plus(), result, mult())
          }
          case Minus() => {
            eat(typeTag[Minus])
            result = new BinOp(new Minus(), result, mult())
          }
        }
      }
      result
    }

    def assignment_statement(): AST = {
        val varToken = currentToken
        eat(typeTag[Id])
        eat(typeTag[AssignToken])
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
        eat(typeTag[SemiColon])
        childStatements += statement()
      }
      new StatementList(childStatements)
    }

    def compound_statement(): AST = {
      eat(typeTag[Begin])
      val result = statement_list()
      eat(typeTag[End])
      result
    }

    def program(): AST = {
      val result = compound_statement()
      eat(typeTag[Dot])
      result
    }

  }
}
