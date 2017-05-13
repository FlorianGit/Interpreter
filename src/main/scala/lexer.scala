package Lexer {

  trait Token
  case class IntToken(n: Int) extends Token
  case class Plus() extends Token
  case class Minus() extends Token
  case class Times() extends Token
  case class Div() extends Token
  case class EOF() extends Token
  case class ParenthesisOpen() extends Token
  case class ParenthesisClose() extends Token
  case class Assign() extends Token
  case class Id(name: String) extends Token

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

    def getWord(): String = {
      var result: String = ""
      while (currentPos < input.length && input(currentPos).isLetter) {
        result += input(currentPos)
        currentPos += 1
      }
      result
    }

    def peek(): Char = if (currentPos >= input.length() - 1) ' ' else input(currentPos + 1)

    def getNextToken(): Token = if (currentPos >= input.length()) new EOF else input(currentPos) match {
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
      case ':' => {
        if (peek() != '=') throw new Exception else
          currentPos += 2
          new Assign()
      }
      case c => {
        if (c.isDigit) {
          new IntToken(getInteger())
        }
        else {
          new Id(getWord())
        }
      }
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
