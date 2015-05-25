package rosettaCode

import scala.collection.immutable.HashMap

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

  val TRACE : Boolean = false
  val TAPE_LENGTH = 32
  type Tape = Array[String]
  type States = List[String]
  type Symbols = List[String]
  type Rules = HashMap[StateSymbolPair, Transition]
  type StateSymbolPair = (String, String)  // (state,symbol)

  val L: String = "L" // Left
  val R: String = "R" // Right
  val S: String = "S" // Stay

  def simpleIncrementer = {
    println("simpleIncrement:")
    val states: States = List[String]("q0", "qf")
    val initialState = "q0"
    val terminatingState = "qf"
    val blank: String = "B"
    val symbols: Symbols = List[String](blank, "1")
    val rules: Rules = HashMap[StateSymbolPair, Transition](
      ("q0", "1")   ->new Transition("1", R, "q0"),
      ("q0", blank) ->new Transition("1", S, "qf")
    )
    val tape: Tape = initializeTape(TAPE_LENGTH, blank, "111", TAPE_LENGTH/2)
    val result = runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
    assert(result.mkString("").equals("BBBBBBBBBBBBBBBB1111BBBBBBBBBBBB"),s"""simpleIncrementer produced wrong result: $result.mkString("")""")
    println("---\n")
  }

  def threeStateBusyBeaver = {
    println("threeStateBusyBeaver:")
    val states: States = List[String]("a", "b", "c", "halt")
    val initialState = "a"
    val terminatingState = "halt"
    val blank: String = "0"
    val symbols: Symbols = List[String]("0", "1")
    val rules: Rules = HashMap[StateSymbolPair, Transition](
      ("a", "0") ->new Transition("1", R, "b"),
      ("a", "1") ->new Transition("1", L, "c"),
      ("b", "0") ->new Transition("1", L, "a"),
      ("b", "1") ->new Transition("1", R, "b"),
      ("c", "0") ->new Transition("1", L, "b"),
      ("c", "1") ->new Transition("1", S, "halt")
    )
    val tape = initializeTape(TAPE_LENGTH, blank)
    val result = runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
    assert(result.mkString("").equals("00000000000001111110000000000000"), s"""threeStateBusyBeaver produced wrong result: $result.mkString("")""")
    println("---\n")
  }

  def sorter = {
    println("sorter")
    val states: States = List[String]("s0", "s1", "s2", "s3", "se", "see")
    val initialState = "s0"
    val terminatingState = "see"
    val blank = "*"
    val symbols: Symbols = List[String]("a", "b", blank, "B")
    val rules: Rules = HashMap[StateSymbolPair, Transition](
      ("s0", "a")   ->new Transition("a", R, "s0"),
      ("s0", "b")   ->new Transition("B", R, "s1"),
      ("s0", blank) ->new Transition(blank, L, "se"),
      ("s1", "a")   ->new Transition("a", R, "s1"),
      ("s1", "b")   ->new Transition("b", R, "s1"),
      ("s1", blank) ->new Transition(blank, L, "s2"),
      ("s2", "a")   ->new Transition("b", L, "s3"),
      ("s2", "b")   ->new Transition("b", L, "s2"),
      ("s2", "B")   ->new Transition("b", L, "se"),
      ("s3", "a")   ->new Transition("a", L, "s3"),
      ("s3", "b")   ->new Transition("b", L, "s3"),
      ("s3", "B")   ->new Transition("a", R, "s0"),
      ("se", "a")   ->new Transition("a", L, "se"),
      ("se", blank) ->new Transition(blank, R, "see")
    )
    val tape = initializeTape(TAPE_LENGTH, blank,"babbababaa",TAPE_LENGTH/2)
    val result = runUTM(states, initialState, terminatingState, symbols, blank, rules, tape)
    assert(result.mkString("").equals("****************aaaaabbbbb******"),s"""sorter produced wrong result: $result.mkString("")""")
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
   * @param states List of valid dates
   * @param initialState The initial state of the UTM
   * @param terminatingState The terminating state of the UTM
   * @param symbols List of valid symbols
   * @param blank The symbol representing a 'blank'
   * @param rules The map of rules
   * @param initialTape The initial state of the tape
   */
  def runUTM(states: States, initialState: String, terminatingState: String, symbols: Symbols,
                 blank: String, rules: Rules, initialTape: Tape): Tape = {

    var currentState = initialState
    var headPosition: Int = initialTape.length / 2
    var tape = initialTape
    var stepCount = 0

    if(TRACE) println(s"terminatingState=$terminatingState")
    while (currentState != terminatingState) {

      if (TRACE) println(s"Step #$stepCount")
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

      val transition = rules.get((currentState, tape(headPosition))).
        getOrElse(throw new RuntimeException(s"transition not found in rules"))
      if (TRACE) println("trans=" + transition)

      tape(headPosition) = transition.symbol

      currentState = transition.state

      transition.movement match {
        case L => headPosition -= 1 // move to the left
        case R => headPosition += 1 // move to the right
        case S => ; // don't move
        case _ => throw new RuntimeException("Bad transition?: " + transition)
      }

      print("tape=")
      printTape(tape, headPosition)
      println("")

    }

    println(s"$stepCount steps")

    tape

  }

  class Transition(val symbol:String,  val movement:String, val state:String) {
    def this(trans:(String,String,String)){
      this(trans._1,trans._2,trans._3)
    }
    override def toString = s"symbol=$symbol movement=$movement state=$state"
  }

  /**
   * Initialize 'blank' tape with fill character.
   * @param length What the length of the tape will be.
   * @param fillChar The fill character for the tape
   * @return Initialized tape
   */
  def initializeTape(length: Int, fillChar: String): Tape = {
    var tape = new Tape(length)
    for (i <- 0 until length) tape(i) = fillChar
    tape
  }

  /**
   * Initialize 'blank' tape with fill character and specified set of symbols
   * at location fillPosition.
   *
   * @param length What the length of the tape will be.
   * @param fillChar The fill character for the tape
   * @param fillString The symbols to place on the tape
   * @param fillPosition The starting position of fillString
   * @return Initialized tape
   */
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
   * Show contents of tape. Put parentheses around position of where the head is.
   * @param tape The tape
   * @param headPosition The current position on the tape of where the head is.
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
   * @param tape The tape to be prepended to.
   * @param fillChar The fill character for prepended portion of the tape.
   * @return tuple of extended tape and current head position.
   */
  def prependTape(tape:Tape,fillChar:String) : (Tape,Int) = {
    val extraTape = initializeTape(tape.length,fillChar)
    val newTape : Tape = extraTape ++ tape
    (newTape,extraTape.length)
  }

  /**
   * Append a tape segment.
   * @param tape The tape to be appended to
   * @param fillChar The fill character for appended portion of the tape.
   * @return tuple of extended tape and current head position.
   */
  def appendTape(tape:Tape,fillChar:String) : (Tape,Int) = {
    val extraTape = initializeTape(tape.length,fillChar)
    val newTape : Tape = tape ++ extraTape
    (newTape,tape.length)
  }

}

//simpleIncrement:
//tape=BBBBBBBBBBBBBBBB1(1)1BBBBBBBBBBBBB
//tape=BBBBBBBBBBBBBBBB11(1)BBBBBBBBBBBBB
//tape=BBBBBBBBBBBBBBBB111(B)BBBBBBBBBBBB
//tape=BBBBBBBBBBBBBBBB111(1)BBBBBBBBBBBB
//4 steps
//---
//
//threeStateBusyBeaver:
//tape=00000000000000001(0)00000000000000
//tape=0000000000000000(1)100000000000000
//tape=000000000000000(0)1100000000000000
//tape=00000000000000(0)11100000000000000
//tape=0000000000000(0)111100000000000000
//tape=00000000000001(1)11100000000000000
//tape=000000000000011(1)1100000000000000
//tape=0000000000000111(1)100000000000000
//tape=00000000000001111(1)00000000000000
//tape=000000000000011111(0)0000000000000
//tape=00000000000001111(1)10000000000000
//tape=0000000000000111(1)110000000000000
//tape=0000000000000111(1)110000000000000
//13 steps
//---
//
//sorter
//tape=****************B(a)bbababaa******
//tape=****************Ba(b)bababaa******
//tape=****************Bab(b)ababaa******
//tape=****************Babb(a)babaa******
//tape=****************Babba(b)abaa******
//tape=****************Babbab(a)baa******
//tape=****************Babbaba(b)aa******
//tape=****************Babbabab(a)a******
//tape=****************Babbababa(a)******
//tape=****************Babbababaa(*)*****
//tape=****************Babbababa(a)******
//tape=****************Babbabab(a)b******
//tape=****************Babbaba(b)ab******
//tape=****************Babbab(a)bab******
//tape=****************Babba(b)abab******
//tape=****************Babb(a)babab******
//tape=****************Bab(b)ababab******
//tape=****************Ba(b)bababab******
//tape=****************B(a)bbababab******
//tape=****************(B)abbababab******
//tape=****************a(a)bbababab******
//tape=****************aa(b)bababab******
//tape=****************aaB(b)ababab******
//tape=****************aaBb(a)babab******
//tape=****************aaBba(b)abab******
//tape=****************aaBbab(a)bab******
//tape=****************aaBbaba(b)ab******
//tape=****************aaBbabab(a)b******
//tape=****************aaBbababa(b)******
//tape=****************aaBbababab(*)*****
//tape=****************aaBbababa(b)******
//tape=****************aaBbabab(a)b******
//tape=****************aaBbaba(b)bb******
//tape=****************aaBbab(a)bbb******
//tape=****************aaBba(b)abbb******
//tape=****************aaBb(a)babbb******
//tape=****************aaB(b)ababbb******
//tape=****************aa(B)bababbb******
//tape=****************aaa(b)ababbb******
//tape=****************aaaB(a)babbb******
//tape=****************aaaBa(b)abbb******
//tape=****************aaaBab(a)bbb******
//tape=****************aaaBaba(b)bb******
//tape=****************aaaBabab(b)b******
//tape=****************aaaBababb(b)******
//tape=****************aaaBababbb(*)*****
//tape=****************aaaBababb(b)******
//tape=****************aaaBabab(b)b******
//tape=****************aaaBaba(b)bb******
//tape=****************aaaBab(a)bbb******
//tape=****************aaaBa(b)bbbb******
//tape=****************aaaB(a)bbbbb******
//tape=****************aaa(B)abbbbb******
//tape=****************aaaa(a)bbbbb******
//tape=****************aaaaa(b)bbbb******
//tape=****************aaaaaB(b)bbb******
//tape=****************aaaaaBb(b)bb******
//tape=****************aaaaaBbb(b)b******
//tape=****************aaaaaBbbb(b)******
//tape=****************aaaaaBbbbb(*)*****
//tape=****************aaaaaBbbb(b)******
//tape=****************aaaaaBbb(b)b******
//tape=****************aaaaaBb(b)bb******
//tape=****************aaaaaB(b)bbb******
//tape=****************aaaaa(B)bbbb******
//tape=****************aaaa(a)bbbbb******
//tape=****************aaa(a)abbbbb******
//tape=****************aa(a)aabbbbb******
//tape=****************a(a)aaabbbbb******
//tape=****************(a)aaaabbbbb******
//tape=***************(*)aaaaabbbbb******
//tape=****************(a)aaaabbbbb******
//72 steps
//---
