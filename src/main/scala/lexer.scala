package Lexer {

  trait Token
  case class IntConst(n: Int) extends Token
  case class FloatConst(x: Double) extends Token
  case class Plus() extends Token
  case class Minus() extends Token
  case class Times() extends Token
  case class IntDiv() extends Token
  case class FloatDiv() extends Token
  case class EOF() extends Token
  case class ParenthesisOpen() extends Token
  case class ParenthesisClose() extends Token
  case class AssignToken() extends Token
  case class Id(name: String) extends Token
  case class Dot() extends Token
  case class SemiColon() extends Token
  case class Begin() extends Token
  case class End() extends Token
  case class Program() extends Token
  case class VarKeyword() extends Token
  case class Comma() extends Token
  case class Colon() extends Token
  case class Integer() extends Token
  case class FloatKeyword() extends Token

  class Lexer(input:String) {
    var currentPos = 0

    def getNumber(): Token = {
      var result: String = ""
      while (currentPos < input.length && input(currentPos).isDigit) {
        result += input(currentPos)
        currentPos += 1
      }
      if (currentPos < input.length && input(currentPos) == '.') {
        result += '.'
        currentPos += 1
        while (currentPos < input.length && input(currentPos).isDigit) {
          result += input(currentPos)
          currentPos += 1
        }
        new FloatConst(result.toFloat)
      }
      else new IntConst(result.toInt)
    }

    def getWord(): String = {
      var result: String = ""
      while (currentPos < input.length && (input(currentPos).isLetter || input(currentPos).isDigit)) {
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
        new FloatDiv()
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
        if (peek() == '=') {
          currentPos += 2
          new AssignToken()
        } else {
          currentPos += 1
          new Colon()
        }
      }
      case ';' => {
        currentPos += 1
        new SemiColon()
      }
      case '.' => {
        currentPos += 1
        new Dot()
      }
      case ',' => {
        currentPos += 1
        new Comma()
      }
      case c if (c.isDigit) => getNumber()
      case c => {
          val nextWord = getWord()
          nextWord match {
            case "BEGIN" => new Begin()
            case "END" => new End()
            case "VAR" => new VarKeyword()
            case "PROGRAM" => new Program()
            case "INTEGER" => new Integer()
            case "FLOAT" => new FloatKeyword()
            case "div" => new IntDiv()
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
