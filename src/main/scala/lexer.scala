package Lexer {

  trait Token
  case class Number(n: Int) extends Token
  case class Plus extends Token
  case class Minus extends Token
  case class Empty extends Token

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

    def getNextToken(): Token = input(currentPos) match {
      case '+' => {
        currentPos += 1
        new Plus()
      }
      case '-' => {
        currentPos += 1
        new Minus()
      }
      case _ => new Number(getInteger())
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
