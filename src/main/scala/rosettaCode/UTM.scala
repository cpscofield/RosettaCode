package rosettaCode

import scala.collection.mutable.{ArrayBuffer}
import scala.collection.immutable.{HashMap}

/**
 * Solution for http://rosettacode.org/wiki/Universal_Turing_machine
 *
 * @author Cary Scofield (carys689 <at> gmail <dot> com) on 5/24/2015.
 * @since Scala 2.11.2
 */
object UTM {
  def main(args: Array[String]): Unit = {
    simpleIncrementer
    threeStateBusyBeaver
    sorter
  }

  val TRACE : Boolean = false;
  val TAPE_LENGTH = 32
  type Tape = Array[String]
  type States = List[String]
  type Symbols = List[String]
  type Rules = HashMap[StateSymbolPair, Transition]
  type StateSymbolPair = (String, String)  // (state,symbol)
  type Transition = (String, String, String)  // (symbol,movement,new state)

  val L: String = "L" // Left
  val R: String = "R" // Right
  val S: String = "S" // Stay

  def simpleIncrementer: Unit = {
    println("simpleIncrement:")
    val states: States = List[String]("q0", "qf")
    val initialState = "q0"
    val terminatingState = "qf"
    val blank: String = "B"
    val symbols: Symbols = List[String](blank, "1")
    val rules: Rules = HashMap[StateSymbolPair, Transition](
      ("q0", "1") ->("1", R, "q0"),
      ("q0", blank) ->("1", S, "qf")
    )
    var tape: Tape = initializeTape(TAPE_LENGTH, blank, "111", TAPE_LENGTH/2)
    runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
    println("---\n")
  }

  def threeStateBusyBeaver: Unit = {
    println("threeStateBusyBeaver:")
    val states: States = List[String]("a", "b", "c", "halt")
    val initialState = "a"
    val terminatingState = "halt"
    val blank: String = "0"
    val symbols: Symbols = List[String]("0", "1")
    val rules: Rules = HashMap[StateSymbolPair, Transition](
      ("a", "0") ->("1", R, "b"),
      ("a", "1") ->("1", L, "c"),
      ("b", "0") ->("1", L, "a"),
      ("b", "1") ->("1", R, "b"),
      ("c", "0") ->("1", L, "b"),
      ("c", "1") ->("1", S, "halt")
    )
    var tape = initializeTape(TAPE_LENGTH, blank)
    runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
    println("---\n")
  }

  def sorter: Unit = {
    println("sorter")
    val states: States = List[String]("s0", "s1", "s2", "s3", "se", "see")
    val initialState = "s0"
    val terminatingState = "see"
    val blank = "*"
    val symbols: Symbols = List[String]("a", "b", blank, "B")
    val rules: Rules = HashMap[StateSymbolPair, Transition](
      ("s0", "a")   ->("a", R, "s0"),
      ("s0", "b")   ->("B", R, "s1"),
      ("s0", blank) ->(blank, L, "se"),
      ("s1", "a")   ->("a", R, "s1"),
      ("s1", "b")   ->("b", R, "s1"),
      ("s1", blank) ->(blank, L, "s2"),
      ("s2", "a")   ->("b", L, "s3"),
      ("s2", "b")   ->("b", L, "s2"),
      ("s2", "B")   ->("b", L, "se"),
      ("s3", "a")   ->("a", L, "s3"),
      ("s3", "b")   ->("b", L, "s3"),
      ("s3", "B")   ->("a", R, "s0"),
      ("se", "a")   ->("a", L, "se"),
      ("se", blank) ->(blank, R, "see")
    )
    var tape = initializeTape(TAPE_LENGTH, blank,"babbababaa",TAPE_LENGTH/2)
    runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
    println("---\n\n")
  }

  /**
   * Don't run this. Supposedly runs for over 47 million steps. Never tried it and don't want to! :)
   */
//  def bonusBusyBeaver: Unit = {
//    println("bonusBusyBeaver")
//    val states: States = List[String]("A", "B", "C", "D", "E", "H")
//    val initialState = "A"
//    val terminatingState = "H"
//    val blank = "0"
//    val symbols: Symbols = List[String]("0", "1")
//    val rules: Rules = HashMap[StateSymbolPair, Transition](
//      ("A", "0") ->("1", R, "B"),
//      ("A", "1") ->("1", L, "C"),
//      ("B", "0") ->("1", R, "C"),
//      ("B", "1") ->("1", R, "B"),
//      ("C", "0") ->("1", R, "D"),
//      ("C", "1") ->("0", L, "E"),
//      ("D", "0") ->("1", L, "A"),
//      ("D", "1") ->("1", L, "D"),
//      ("E", "0") ->("1", S, "H"),
//      ("E", "1") ->("0", L, "A")
//    )
//    var tape: Tape = initializeTape(TAPE_LENGTH, blank)
//    runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
//    println("---\n\n")
//  }


  /**
   * Run the Univeral Turing Machine.
   * @param states
   * @param initialState
   * @param terminatingState
   * @param symbols
   * @param blank
   * @param rules
   * @param initialTape
   */
  def runUTM(states: States, initialState: String, terminatingState: String, symbols: Symbols,
                 blank: String, rules: Rules, initialTape: Tape): Unit = {

    var currentState = initialState
    var headPosition: Int = initialTape.length / 2
    var tape = initialTape
    var stepCount = 0

    if(TRACE) println(s"terminatingState=$terminatingState")
    while (currentState != terminatingState) {

      if (TRACE) println(s"Step #$stepCount");
      stepCount += 1

      if (headPosition < 0) {
        // Tape needs to be extended on the left
        val newVals = prependTape(tape, blank)
        tape = newVals._1
        headPosition = newVals._2
      }
      else if (headPosition >= tape.length) {
        // Tape needs to be extended on the right
        val newVals = appendTape(tape, blank)
        tape = newVals._1
        headPosition = newVals._2
      }

      if (TRACE) {
        println(s"currentState=$currentState headPosition=$headPosition ")
        print("tape(before)=")
        printTape(tape, headPosition)
        println("")
      }

      var transition = rules.get((currentState, tape(headPosition)))
      if(TRACE) println("trans=" + printTransition(transition))

      replaceSymbol(tape, headPosition, transition)

      getMovement(transition) match {
        case L => headPosition -= 1 // move to the left
        case R => headPosition += 1 // move to the right
        case S => ; // else don't move
        case _ => throw new RuntimeException("Bad transition?: " + printTransition(transition))
      }

      currentState = getState(transition)

      print("tape=")
      printTape(tape, headPosition)
      println("")

    }

    println(s"$stepCount steps")

  }

  /**
   * Initialize 'blank' tape with fill character.
   * @param length
   * @param fillChar
   * @return
   */
  def initializeTape(length: Int, fillChar: String): Tape = {
    var tape = new Tape(length)
    for (i <- 0 until length) tape(i) = fillChar
    tape
  }

  def initializeTape(length: Int, fillChar: String, fillString: String, fillPosition: Int) : Tape = {
    var tape = initializeTape(length,fillChar)
    var middle = fillPosition
    var j = 0
    for(i <- middle until middle + fillString.length) {
      tape(i) = fillString(j).toString
      j += 1
    }
    tape
  }

  /**
   * Show contents of transition tuple.
   * @param transition
   */
  def printTransition(transition: Option[Transition]): Unit = {
    val trans = transition.getOrElse(("", "", ""))
    val symbol = trans._1
    val movement = trans._2
    val state = trans._3
    println(s"trans=($symbol,$movement,$state)")
  }

  /**
   * Extract symbol from transition tuple.
   * @param transition
   * @return symbol
   */
  def getSymbol(transition: Option[Transition]): String = {
    transition.getOrElse(("", "", ""))._1
  }

  /**
   * Extract movement direction from transition tuple.
   * @param transition
   * @return movement
   */
  def getMovement(transition: Option[Transition]): String = {
    transition.getOrElse(("", "", ""))._2
  }

  /**
   * Extract state from transition tuple.
   * @param transition
   * @return state
   */
  def getState(transition: Option[Transition]): String = {
    transition.getOrElse(("", "", ""))._3
  }

  /**
   * Replace simple at specified position in tape.
   * @param tape
   * @param position
   * @param transition
   */
  def replaceSymbol(tape: Tape, position: Int, transition: Option[Transition]): Unit = {
    val symbol = getSymbol(transition)
    if(TRACE) println( s"Putting $symbol at position=$position")
    tape(position) = symbol
  }

  /**
   * Show contents of tape. Put parentheses around position of where the head is.
   * @param tape
   * @param headPosition
   */
  def printTape(tape: Tape, headPosition: Int): Unit = {
    for (t <- 0 until tape.length) {
      val c: String = tape(t)
      if (t == headPosition) {
        print( s"""($c)""")
      }
      else {
        print(s"$c")
      }
    }
  }


  /**
   * Prepend a tape segment.
   * @param tape
   * @param fillChar
   * @return tuple of extended tape and current head position.
   */
  def prependTape(tape:Tape,fillChar:String) : (Tape,Int) = {
    val extraTape = initializeTape(tape.length,fillChar)
    val newTape : Tape = extraTape ++ tape
    (newTape,extraTape.length)
  }

  /**
   * Append a tape segment.
   * @param tape
   * @param fillChar
   * @return tuple of extended tape and current head position.
   */
  def appendTape(tape:Tape,fillChar:String) : (Tape,Int) = {
    val extraTape = initializeTape(tape.length,fillChar)
    val newTape : Tape = tape ++ extraTape
    (newTape,tape.length)
  }
}

