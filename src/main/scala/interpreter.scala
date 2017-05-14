import scala.collection.mutable.HashMap
import Lexer._
import Parser._

package Interpreter {
  class Interpreter(parser: Parser) {
    var varTable = new HashMap[String, Int]

    def releaseNumber(t: Token) = t match {
      case IntConst(n) => n
      case _ => 0
    }

    def releaseName(t: Token) = t match {
      case Id(name) => name
      case _ => throw new Exception()
    }

    def store(key: String, value: Int) = {
      varTable += ( key -> value )
    }

    def lookup(key: String): Int = {
      varTable(key)
    }

    def evaluate(tree: AST): Int = tree match {
      case Number(t) => releaseNumber(t)
      case BinOp(t, left, right) => t match {
        case Plus() => evaluate(left) + evaluate(right)
        case Minus() => evaluate(left) - evaluate(right)
        case Times() => evaluate(left) * evaluate(right)
        case Div() => evaluate(left) / evaluate(right)
      }
        case Var(id) => lookup(releaseName(id))
    }

    def interpret(tree: AST): Unit = tree match {
      case Assign(id, rhs) => store(releaseName(id), evaluate(rhs))
      case StatementList(statements) => statements map interpret
      case NoOp() => ()
    }

    def printVarTable() =  {
      interpret(parser.program())
      varTable.toString
    }
  }
}

