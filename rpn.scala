def eval(string: String): Double =
  val commands = string.split(" ").toList
  
  def op(stack: List[Double], command: String): List[Double] = 
    (stack, command) match
      case (x :: y :: rest, "+") => (y + x) :: rest
      case (x :: y :: rest, "-") => (y - x) :: rest
      case (x :: y :: rest, "/") => (y / x) :: rest
      case (x :: y :: rest, "*") => (y * x) :: rest
      case (stack, number) => number.toDouble :: stack
  
  val resultStack = commands.foldLeft(List[Double]())(op)
  
  resultStack match
    case x :: Nil => x
    case _ => throw new Exception("unexpected final stack state")