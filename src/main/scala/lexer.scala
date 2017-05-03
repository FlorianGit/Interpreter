object Lexer {

  trait Token

  case class Number(n: Int) extends Token
  case class Plus extends Token
  case class Minus extends Token
  case class Empty extends Token

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

  trait Phrase
  case class PNumber(n: Int) extends Phrase
  case class PPlus(a: Phrase, b: Phrase) extends Phrase
  case class PMinus(a: Phrase, b: Phrase) extends Phrase
  case class PEmpty extends Phrase

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
}
