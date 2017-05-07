import Lexer._
import Parser._

class Interpreter() {
  def releaseNumber(t: Token) = t match {
    case IntToken(n) => n
    case _ => 0
  }

  def evaluate(tree: AST): Int = tree match {
    case Number(t) => releaseNumber(t)
    case BinOp(t, left, right) => t match {
      case Plus() => evaluate(left) + evaluate(right)
      case Minus() => evaluate(left) - evaluate(right)
      case Times() => evaluate(left) * evaluate(right)
      case Div() => evaluate(left) / evaluate(right)
    }
  }
}



