package rosettaCode


/**
 * Solution for http://rosettacode.org/wiki/Parsing/RPN_calculator_algorithm
 *
 * If the program runs correctly, there should be no output (i.e., all assertions succeed)
 *
 * @author Cary Scofield (carys689 <at> gmail <dot> com) on 5/21/2015.
 * @since Scala 2.11.2
 */
object RPN {
  def main(args: Array[String]): Unit = {
    try {
      assert(solve("3 5 +") == 8, "3 5 + != 8")
      assert(solve("3 5 *") == 15, "3 5 * != 15")
      assert(solve("3 6 /") == 2, "3 6 / != 2")
      assert(solve("5 3 -") == -2, "5 3 - != -2")
      assert(solve("3 2 ^") == 8, "3 2 ^ != 8")
      assert(farenheitToCelsius(32) == 0, "farentheitToCelsius(32) != 0")
      assert(isEqual(farenheitToCelsius(0), -17.7778), "farenheitToCelsius(0) != -17.7778")
      assert(celsiusToFarenheit(0) == 32, "celsiusToFarenheit(0) != 32")
      assert(isEqual(celsiusToFarenheit(-32), -25.6), "celsiusToFarenheit(-32) != -25.6")
    }
    catch {
      case e: AssertionError => println(e.toString)
      case e: Exception => e.printStackTrace()
    }
  }

  // Test if two floating-point numbers are 'equal'
  def isEqual(actual: Double, expected: Double): Boolean = {
    val EPSILON: Double = 0.0001
    math.abs(actual - expected) < EPSILON
  }

  def solve(equation: String): Double = {
    evaluate(equation.split(" ").toList)
  }

  def farenheitToCelsius(ftemp: Double): Double = {
    solve(s"9 5 / 32 $ftemp - *")
  }

  def celsiusToFarenheit(ctemp: Double): Double = {
    solve(s"$ctemp 5 9 / * 32 +")
  }

  def evaluate(tokens: List[String]): Double = {
    import scala.collection.mutable.Stack
    val stack: Stack[Double] = new Stack[Double]
    for( token <- tokens ) {
      if (isOperator(token)) token match {
        case "+" => stack.push(stack.pop + stack.pop)
        case "-" => stack.push(stack.pop - stack.pop)
        case "*" => stack.push(stack.pop * stack.pop)
        case "/" => stack.push(stack.pop / stack.pop)
        case "^" => stack.push(math.pow(stack.pop, stack.pop))
        case _ => throw new RuntimeException( s""""$token" is not an operator""")
      }
      else stack.push(token.toDouble)
    }
    stack.pop
  }

  def isOperator(token: String): Boolean = {
    token match {
      case "+" => true; case "-" => true; case "*" => true; case "/" => true; case "^" => true
      case _ => false
    }
  }
}
