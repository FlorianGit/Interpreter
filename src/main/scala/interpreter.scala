object Interpreter {

  abstract class Expr {
    def evaluate(): Integer
  }

  case class Times(left:Expr, right: Expr) extends Expr {
    def evaluate() = left.evaluate * right.evaluate
  }

  case class Plus(left: Expr, right: Expr) extends Expr {
    def evaluate() = left.evaluate + right.evaluate
  }

  case class Minus(left: Expr, right: Expr) extends Expr {
     def evaluate() = left.evaluate - right.evaluate
  }

  case class Number(x: Integer) extends Expr {
    def evaluate() = x
  }

  def interpret(text: String): Expr = {
    if (text contains '+') {
      val splittedAtPlus = text split '+'
      val tokens = splittedAtPlus map interpret
      tokens reduce (Plus(_, _))
    }
    else if (text contains '-') {
       val splittedAtMinus = text split '-'
       val tokens = splittedAtMinus map interpret
       tokens reduce (Minus(_,_))
    }
    else if (text contains '*') {
      val splittedAtTimes = text split '*'
      val tokens = splittedAtTimes map interpret
      tokens reduce (Times(_: Expr, _: Expr))
    }
    else Number(text.toInt)
  }

}