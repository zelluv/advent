import scala.annotation.tailrec
import scala.collection.immutable.{ Queue }
import scala.collection.mutable.{ ListBuffer }

// --- Day 10: Elves Look, Elves Say ---
//
// Today, the Elves are playing a game called look-and-say. They take turns
// making sequences by reading aloud the previous sequence and using that
// reading as the next sequence. For example, 211 is read as "one two, two
// ones", which becomes 1221 (1 2, 2 1s).
//
// Look-and-say sequences are generated iteratively, using the previous value as
// input for the next step. For each step, take the previous value, and replace
// each run of digits (like 111) with the number of digits (3) followed by the
// digit itself (1).
//
// For example:
//
// - 1 becomes 11 (1 copy of digit 1).
// - 11 becomes 21 (2 copies of digit 1).
// - 21 becomes 1211 (one 2 followed by one 1).
// - 1211 becomes 111221 (one 1, one 2, and two 1s).
// - 111221 becomes 312211 (three 1s, two 2s, and one 1).
//
// Starting with the digits in your puzzle input, apply this process 40 times.
// What is the length of the result?

// Your puzzle input is 1321131112. 
// 40 times: 492982
// 50 times: 6989950 

object LookAndSay {

  //fastest
  def encode_find(s: String): String = {
    val pattern = "([1-3])(\\1*)".r
    val result = StringBuilder.newBuilder
    pattern.findAllIn(s).matchData.foreach { e =>
      result.append(e.group(2).length + 1)
      result.append(e.group(1))
    }
    result.toString
  }

  def encode(s: String): String = {
    val pattern = "([1-3])(\\1*)".r
    pattern.replaceAllIn(s, m => ((m.group(2).length + 1).toString + m.group(1).toString))
  }

  def encodeTimes(s: String, n: Int): String = {
    (1 to n).foldLeft(s)((result, _i) => encode(result))
  }
}

object Day10 {
  def time[F](f: => F) = {
    val t0 = System.nanoTime
    val ans = f
    printf("Elapsed: %.3f\n",1e-9*(System.nanoTime-t0))
    ans
  }

  val input = "1321131112"
  def solve(): Int = {
    time{LookAndSay.encodeTimes(input, 40).length}
  }

  def solvePart2(): Int = {
    time{LookAndSay.encodeTimes(input, 50).length}
  }
}
