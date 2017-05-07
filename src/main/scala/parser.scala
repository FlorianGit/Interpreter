import Lexer._

package Parser {
  trait AST
  case class BinOp(op: Token, left: AST, right: AST) extends AST
  case class Number(n: Token) extends AST

  class Parser(lexer: Lexer) {
    trait Phrase
    case class PIntToken(n: Int) extends Phrase
    case class PPlus(a: Phrase, b: Phrase) extends Phrase
    case class PMinus(a: Phrase, b: Phrase) extends Phrase
    case class PEmpty extends Phrase

    var currentToken = lexer.getNextToken()

    //def Parse(input: List[Token]): Phrase = {

      //def helper(p: Phrase, rem: List[Token]): Phrase = if (rem.isEmpty) p else rem.head match {
        //case IntToken(n) => PIntToken(n)
        //case Plus() => rem.tail.head match {
          //case IntToken(m) => helper(new PPlus(p, new PIntToken(m)), rem.tail.tail)
          //case _ => new PEmpty
        //}
          //case Minus() => rem.tail.head match {
            //case IntToken(m) => helper(new PMinus(p, new PIntToken(m)), rem.tail.tail)
            //case _ => new PEmpty
          //}
      //}

      //if (input.isEmpty) new PEmpty() else input.head match {
        //case IntToken(n) => helper(PIntToken(n), input.tail)
      //}
    //}

    def eat(tokenType: String) = currentToken match {
      case Plus() => if (tokenType == "PLUS") currentToken = lexer.getNextToken() else throw new Exception()
      case Minus() => if (tokenType == "MINUS") currentToken = lexer.getNextToken() else throw new Exception()
      case Times() => if (tokenType == "TIMES") currentToken = lexer.getNextToken() else throw new Exception()
      case Div() => if (tokenType == "DIV") currentToken = lexer.getNextToken() else throw new Exception()
      case ParenthesisOpen() => if (tokenType == "PARENTHOPEN") currentToken = lexer.getNextToken() else throw new Exception()
      case ParenthesisClose() => if (tokenType == "PARENTHCLOSE") currentToken = lexer.getNextToken() else throw new Exception()
      case _ => currentToken = lexer.getNextToken()
    }

    def factor(): AST = currentToken match {
      case ParenthesisOpen() => {
        eat("PARENTHOPEN")
        val ret = expr()
        eat("PARENTHCLOSE")
        ret
      }
      case _ => {
        val token = currentToken
        eat("INTEGER")
        new Number(token)
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
  }
}
