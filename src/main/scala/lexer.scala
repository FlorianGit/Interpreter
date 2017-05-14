package Lexer {

  trait Token
  case class IntConst(n: Int) extends Token
  case class Plus() extends Token
  case class Minus() extends Token
  case class Times() extends Token
  case class Div() extends Token
  case class EOF() extends Token
  case class ParenthesisOpen() extends Token
  case class ParenthesisClose() extends Token
  case class AssignToken() extends Token
  case class Id(name: String) extends Token
  case class Dot() extends Token
  case class SemiColon() extends Token
  case class Begin() extends Token
  case class End() extends Token

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
      case ' ' | '\n' => {
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
          new AssignToken()
      }
      case ';' => {
        currentPos += 1
        new SemiColon()
      }
      case '.' => {
        currentPos += 1
        new Dot()
      }
      case c if (c.isDigit) => new IntConst(getInteger())
      case c => {
          val nextWord = getWord()
          nextWord match {
            case "BEGIN" => new Begin()
            case "END" => new End()
            case _ => new Id(nextWord)
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
