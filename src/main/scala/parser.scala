import Lexer._

class Parser {
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
