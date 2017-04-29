object Calculator {

  trait Expr {
    def evaluate(): Integer
  }

  type BinaryOperationTag = (Char, String)
  val ops = Array(('+', "Plus"), ('-', "Minus"), ('*', "Times"))

  def BinaryOperationFactory(str: String, left: Expr, right: Expr): Expr = str match {
    case "Plus" => new Plus(left, right)
    case "Minus" => new Minus(left,right)
    case "Times" => new Times(left, right)
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


  def interpret(rawText: String): Expr = {
    val text = rawText.trim
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

  def interpret2(rawText:String) = {
    def interpretHelper(rawText:String, ops: Array[BinaryOperationTag]): Expr = {
      val text = rawText.trim
      if (ops.isEmpty) Number(rawText.trim.toInt) else {
        def op = ops.head
        if (text contains op._1) {
          val splitted = text split op._1
          val tokens = splitted.map(interpretHelper(_, ops.tail))
          tokens reduce ((x,y) => BinaryOperationFactory(op._2, x, y))
        }
        else interpretHelper(rawText, ops.tail)
      }
    }

    interpretHelper(rawText, ops)
  }
}
