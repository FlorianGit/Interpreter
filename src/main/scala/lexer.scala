package Lexer {

  trait Token
  case class Number(n: Int) extends Token
  case class Plus extends Token
  case class Minus extends Token
  case class Empty extends Token

  class Lex {
    def Lex(input: String): List[Token] = {
      val operations = Seq('+', '-')

      def LexSingleToken(input: String): (Token, String) = {
        if (input.isEmpty) (new Empty(), "") else if (operations contains input.head) {
          input.head match {
            case '+' => return (new Plus(), input.tail) 
            case '-' => return (new Minus(), input.tail)
          }
        } else {
          var tmp = input
          var n = 0
          while (!tmp.isEmpty && tmp.head.isDigit) {
            n *= 10
            n += tmp.head.asDigit
            tmp = tmp.tail
          }
          return (new Number(n), tmp)
        }
      }

      var tmp = input
      var output = Nil : List[Token]
      while (!tmp.isEmpty) { 
        val y = LexSingleToken(tmp)
        tmp = y._2
        output = y._1 :: output
      }
      output.reverse
    } 
  }

}
