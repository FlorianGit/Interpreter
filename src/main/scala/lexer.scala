package Lexer {

  trait Token
  case class IntToken(n: Int) extends Token
  case class Plus extends Token
  case class Minus extends Token
  case class Times extends Token
  case class Div extends Token
  case class Empty extends Token
  case class ParenthesisOpen extends Token
  case class ParenthesisClose extends Token

  class Lexer(input:String) {
    var currentPos = 0

    def getInteger() = {
      var result: String = ""
      while (currentPos < input.length && input(currentPos).isDigit) {
        result += input(currentPos)
        currentPos += 1
      }
      result.toInt
    }

    def getNextToken(): Token = if (currentPos >= input.length()) new Empty else input(currentPos) match {
      case ' ' => {
        currentPos += 1
        getNextToken()
      }
      case '+' => {
        currentPos += 1
        new Plus()
      }
      case '-' => {
        currentPos += 1
        new Minus()
      }
      case '*' => {
        currentPos += 1
        new Times()
      }
      case '/' => {
        currentPos += 1
        new Div()
      }
      case '(' => {
        currentPos += 1
        new ParenthesisOpen()
      }
      case ')' => {
        currentPos += 1
        new ParenthesisClose()
      }
      case _ => new IntToken(getInteger())
    }

    def Lex(): List[Token] = {
      var result = Nil : List[Token]
      while (currentPos < input.length()) {
        result = getNextToken() :: result
      }
      result.reverse
    }

  }
}
