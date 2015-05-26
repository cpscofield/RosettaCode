package rosettaCode

import scala.collection.mutable.{Stack}
import scala.collection.immutable.{HashMap}

/**
 * This is more-or-less a transcription of the C++ program
 * at https://gist.github.com/mycodeschool/7867739
 *
 * @author Cary Scofield (carys689 <at> gmail <dot> com) on 5/26/2015.
 * @since Scala 2.11.2
 */
object InfixToPostfix {

  val TRACE = true
  val OPEN_PAREN = "("
  val CLOSE_PAREN = ")"

  def main( args:Array[String]): Unit = {
    val postfix = convertToPostfix("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
    println(s"postfix=$postfix")
    assert(postfix.equals("3 4 2 * 1 5 - 2 3 ^ ^ / +"), "postfix string does not match expected result")
  }

  def convertToPostfix(infix: String): String = {
    var opStack = new Stack[String]()
    var postfix: StringBuilder = new StringBuilder
    val tokens: List[String] = infix.split(" ").toList
    for (token <- tokens) {
      if (isOperator(token)) {
        while( !opStack.isEmpty && opStack.top != OPEN_PAREN && isHigherPrecedence(opStack.top, token) ) {
          postfix.append(opStack.pop).append(" ")
        }
        opStack.push(token)
      }
      else if(token.equals(OPEN_PAREN)) {
        opStack.push(token)
      }
      else if(token.equals(CLOSE_PAREN)) {
        while(!opStack.isEmpty && opStack.top != OPEN_PAREN) {
          postfix.append(opStack.pop).append(" ")
        }
        opStack.pop
      }
      else {
        // token is an operand
        postfix.append(token).append(" ")
      }
    }
    while(!opStack.isEmpty) {
      postfix.append(opStack.pop).append(" ")
    }
    postfix.toString.trim
  }

  def isOperator(token:String) : Boolean = {
    token match {
      case "*" => true
      case "/" => true
      case "+" => true
      case "-" => true
      case "^" => true
      case _   => false
    }
  }

  lazy val opWeights = HashMap[String, Int](
    "^" -> 3, "*" -> 2, "/" -> 2, "+" -> 1, "-" -> 1
  )

  def isHigherPrecedence(stackOp:String, token:String) : Boolean = {
    def isRightAssociative(token:String):Boolean = token.equals("^")
    def throwEx = throw new RuntimeException("Unrecognized operator")
    val stackOpWeight:Int = opWeights.getOrElse(stackOp,throwEx)
    val tokenOpWeight:Int = opWeights.getOrElse(token,throwEx)
    if(stackOpWeight > tokenOpWeight) true
    else if(stackOpWeight < tokenOpWeight) false
    else if(isRightAssociative(token)) false
    else true
  }



}
