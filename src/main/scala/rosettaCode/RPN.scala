package rosettaCode


/**
 * Solution for http://rosettacode.org/wiki/Parsing/RPN_calculator_algorithm
 *
 * If the program runs correctly, there should be no assertion failures.
 *
 * @author Cary Scofield (carys689 <at> gmail <dot> com) on 5/21/2015.
 * @since Scala 2.11.2
 */
object RPN {
  val PRINT_STACK_CONTENTS: Boolean = false

  def main(args: Array[String]): Unit = {
    try {
      assert(isEqual(solve("3 4 2 * 1 5 - 2 3 ^ ^ / +"), 3.00012)) // from http://rosettaCode.org
      assert(isEqual(solve("3 5 +"), 8), "3 5 + != 8")
      assert(isEqual(solve("3 5 *"), 15), "3 5 * != 15")
      assert(isEqual(solve("6 3 /"), 2), "3 6 / != 2")
      assert(isEqual(solve("3 5 -"), -2), "5 3 - != -2")
      assert(isEqual(solve("2 3 ^"), 8), "3 2 ^ != 8")
      assert(isEqual(fahrenheitToCelsius(32), 0), "fahrenheitToCelsius(32) != 0")
      assert(isEqual(fahrenheitToCelsius(0), -17.7778), "fahrenheitToCelsius(0) != -17.7778")
      assert(isEqual(celsiusToFahrenheit(0), 32), "celsiusToFahrenheit(0) != 32")
      assert(isEqual(celsiusToFahrenheit(-32), -25.6), "celsiusToFahrenheit(-32) != -25.6")
    }
    catch {
      case e: AssertionError => println(e.toString)
      case e: Exception => e.printStackTrace()
    }
  }

  // Test if two floating-point numbers are 'equal' within a specified level of precision
  def isEqual(actual: Double, expected: Double): Boolean = {
    val EPSILON: Double = 0.0001
    math.abs(actual - expected) < EPSILON
  }

  def solve(equation: String): Double = {
    evaluate(equation.split(" ").toList)
  }

  def fahrenheitToCelsius(ftemp: Double): Double = {
    // C = (F - 32) * 5 / 9
    solve(s"5 9 / $ftemp 32 - *")
  }

  def celsiusToFahrenheit(ctemp: Double): Double = {
    // F = C * 9 / 5 + 32
    solve(s"$ctemp 9 5 / * 32 +")
  }

  def evaluate(tokens: List[String]): Double = {
    import scala.collection.mutable.Stack
    val stack: Stack[Double] = new Stack[Double]
    for (token <- tokens) {
      if (isOperator(token)) token match {
        case "+" => stack.push(stack.pop + stack.pop)
        case "-" => val x = stack.pop; stack.push(stack.pop - x)
        case "*" => stack.push(stack.pop * stack.pop)
        case "/" => val x = stack.pop; stack.push(stack.pop / x)
        case "^" => val x = stack.pop; stack.push(math.pow(stack.pop, x))
        case _ => throw new RuntimeException( s""""$token" is not an operator""")
      }
      else stack.push(token.toDouble)

      if (PRINT_STACK_CONTENTS) {
        print("Input: " + token)
        print(" Stack: ")
        for (element <- stack.seq.reverse) print(element + " ");
        println("")
      }
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
