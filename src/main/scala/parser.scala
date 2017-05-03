import Lexer._

class Parser(lexer: Lexer) {
  trait Phrase
  case class PNumber(n: Int) extends Phrase
  case class PPlus(a: Phrase, b: Phrase) extends Phrase
  case class PMinus(a: Phrase, b: Phrase) extends Phrase
  case class PEmpty extends Phrase

  var currentToken = lexer.getNextToken()

  def Parse(input: List[Token]): Phrase = {

    def helper(p: Phrase, rem: List[Token]): Phrase = if (rem.isEmpty) p else rem.head match {
      case Number(n) => PNumber(n)
      case Plus() => rem.tail.head match {
        case Number(m) => helper(new PPlus(p, new PNumber(m)), rem.tail.tail)
        case _ => new PEmpty
      }
        case Minus() => rem.tail.head match {
          case Number(m) => helper(new PMinus(p, new PNumber(m)), rem.tail.tail)
          case _ => new PEmpty
        }
    }

    if (input.isEmpty) new PEmpty() else input.head match {
      case Number(n) => helper(PNumber(n), input.tail)
    }
  }

  def eat(tokenType: String) = currentToken match {
    case Plus() => if (tokenType == "PLUS") currentToken = lexer.getNextToken() else throw new Exception()
    case Minus() => if (tokenType == "MINUS") currentToken = lexer.getNextToken() else throw new Exception()
    case Times() => if (tokenType == "TIMES") currentToken = lexer.getNextToken() else throw new Exception()
    case Div() => if (tokenType == "DIV") currentToken = lexer.getNextToken() else throw new Exception()
    case _ => currentToken = lexer.getNextToken()
  }

  def factor(): Int = {
    val token = currentToken
    eat("INTEGER")
    token match { case Number(n) => n }
  }

  def mult(): Int = {
    def isMultDiv(t: Token) = t match {
      case Times() | Div() => true
      case _ => false
    }

    var result = factor()
    while (isMultDiv(currentToken)) {
      currentToken match {
        case Times() => {
          eat("TIMES")
          result *= factor()
        }
        case Div() => {
          eat("DIV")
          result /= factor()
        }
      }
    }
    result
  }

  def expr(): Int = {
    def isPlusMinus(t: Token) = t match {
      case Plus() | Minus() => true
      case _ => false
    }

    var result = mult()
    while (isPlusMinus(currentToken)) {
      currentToken match {
        case Plus() => {
          eat("PLUS")
          result += mult()
        }
        case Minus() => {
          eat("MINUS")
          result -= mult()
        }
      }
    }
    result
  }
}
