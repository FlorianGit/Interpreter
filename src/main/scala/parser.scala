import scala.collection.mutable.ArrayBuffer
import reflect.ClassTag
import Lexer._

package Parser {
  sealed trait AST
  case class Prog(name: AST, block: AST) extends AST
  case class Block(decl: AST, comp_stat: AST) extends AST
  case class BinOp(op: Token, left: AST, right: AST) extends AST
  case class Number(n: Token) extends AST
  case class Assign(id: Token, rhs: AST) extends AST
  case class Var(id: Token) extends AST
  case class StatementList(statements: ArrayBuffer[AST]) extends AST
  case class NoOp() extends AST
  case class Declaration(ids: ArrayBuffer[Token], vartype: Token) extends AST
  case class DeclarationList(declarations: ArrayBuffer[Declaration]) extends AST

  class Parser(lexer: Lexer) {
    var currentToken = lexer.getNextToken()

    def eat[T](implicit T: ClassTag[T]) =
      if (T.runtimeClass == currentToken.getClass) currentToken = lexer.getNextToken() else throw new Exception(T.runtimeClass.toString ++ " Expected. Received:" ++ currentToken.getClass.toString)

    def currentIs[T](implicit T: ClassTag[T]) = 
      if (T.runtimeClass == currentToken.getClass) true else false

    def empty(): AST = new NoOp()

    def variable(): AST = {
          val token = currentToken
          eat[Id]
          new Var(token)
    }

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
      case FloatConst(_) => {
        val token = currentToken
        eat[FloatConst]
        new Number(token)
      }
      case _  => {
          variable()
        }
    }

    def mult(): AST = {
      def isMultDiv(t: Token) = t match {
        case Times() | IntDiv() | FloatDiv() => true
        case _ => false
      }

      var result = factor()
      while (isMultDiv(currentToken)) {
        currentToken match {
          case Times() => {
            eat[Times]
            result = new BinOp(new Times(), result, factor())
          }
          case IntDiv() => {
            eat[IntDiv]
            result = new BinOp(new IntDiv(), result, factor())
          }
          case FloatDiv() => {
            eat[FloatDiv]
            result = new BinOp(new FloatDiv(), result, factor())
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
      var childStatements = ArrayBuffer(statement())
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

    def type_spec(): Token = {
      val token = currentToken
      currentToken match {
        case Integer() => eat[Integer]
        case FloatKeyword() => eat[FloatKeyword]
      }
      token
    }


    def variable_declaration(): Declaration = {
      var tokens = ArrayBuffer(currentToken)
      eat[Id]
      while (currentIs[Comma]) {
        eat[Comma]
        tokens += currentToken
        eat[Id]
      }
      eat[Colon]
      new Declaration(tokens, type_spec())
    }

    def declarations(): AST = {
      if (currentIs[VarKeyword]) {
          eat[VarKeyword]
          var child_declarations = ArrayBuffer[Declaration](variable_declaration())
          eat[SemiColon]
          while (currentIs[Id]) {
            child_declarations += variable_declaration()
            eat[SemiColon]
        }
        new DeclarationList(child_declarations)
      } else {
        empty()
      }
    }

    def block(): AST = {
      new Block(declarations(),compound_statement() )
    }

    def program(): AST = {
      eat[Program]
      val name = variable()
      eat[SemiColon]
      val bl = block()
      eat[Dot]
      new Prog(name, bl)
    }

  }
}
