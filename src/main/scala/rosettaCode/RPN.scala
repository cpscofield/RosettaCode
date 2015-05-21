package rosettaCode

import java.util.StringTokenizer
import scala.collection.mutable.Stack

/**
 * Solution for http://rosettacode.org/wiki/Parsing/RPN_calculator_algorithm
 *
 * If the program runs correctly, there should be no output (i.e., all assertions succeed)
 *
 * @author Cary Scofield (carys689 <at> gmail <dot> com) on 5/21/2015.
 * @since Scala 2.11.2; Java 1.7
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
      assert(equal(farenheitToCelsius(0), -17.7778), "farenheitToCelsius(0) != -17.7778")
      assert(celsiusToFarenheit(0) == 32, "celsiusToFarenheit(0) != 32")
      assert(equal(celsiusToFarenheit(-32), -25.6), "celsiusToFarenheit(-32) != -25.6")
    }
    catch {
      case e: AssertionError => println(e.toString)
      case e: Exception => e.printStackTrace()
    }
  }

  def equal(actual: Double, expected: Double): Boolean = {
    val EPSILON: Double = 0.0001
    java.lang.Math.abs(actual - expected) < EPSILON
  }

  def solve(equation: String): Double = {
    evaluate(new StringTokenizer(equation, " "))
  }

  def farenheitToCelsius(ftemp: Double): Double = {
    solve(s"9 5 / 32 $ftemp - *")
  }

  def celsiusToFarenheit(ctemp: Double): Double = {
    solve(s"$ctemp 5 9 / * 32 +")
  }

  def evaluate(st: StringTokenizer): Double = {
    val stack: Stack[Double] = new Stack[Double]
    while (st.hasMoreTokens) {
      val token = st.nextToken.trim
      if (isOperator(token)) evaluate(token, stack)
      else stack.push(java.lang.Double.parseDouble(token))
    }
    stack.pop
  }

  def evaluate(operator: String, stack: Stack[Double]): Unit = {
    operator match {
      case "+" => stack.push(stack.pop + stack.pop)
      case "-" => stack.push(stack.pop - stack.pop)
      case "*" => stack.push(stack.pop * stack.pop)
      case "/" => stack.push(stack.pop / stack.pop)
      case "^" => stack.push(java.lang.Math.pow(stack.pop, stack.pop))
      case _   => throw new RuntimeException(s""""$operator" is not an operator""")
    }
  }

  def isOperator(token: String): Boolean = {
    token match {
      case "+" => true; case "-" => true; case "*" => true; case "/" => true; case "^" => true
      case _ => false
    }
  }
}
