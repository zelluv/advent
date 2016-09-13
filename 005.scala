import scala.io.Source
import scala.collection.mutable.{ ListBuffer }

/**
 *   Day 5: nice string is one with all of the following properties:
 */

object StringParser {

  def evaluate(s: String)(v1: Int, v2: Int) = s match {
      case "==" => (v1 == v2)
      case ">" => (v1 > v2)
      case "<" => (v1 < v2)
  }

  def filterLines(s: List[String], pattern: scala.util.matching.Regex, op: String, qty: Int): List[String] = {
    var result = new ListBuffer[String]()
    s.foreach { line =>
      if (evaluate(op)(pattern.findAllIn(line).length, qty)) { 
        result += line
      }
    }
    result.toList
  }

  def readInput = {
    val file = Source.fromFile("input") 
    file.getLines.toList
  }

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }

  def main(args: Array[String]): Unit = {
    val listOfLines = readInput
    //part 1
    //contains at least three vowels (aeiou only)
    val pattern1 = "([aeiou])".r
    //contains at least one letter that appears twice in a row
    val pattern2 = "([a-zA-Z])\\1".r
    //does not contain the strings ab, cd, pq, or xy
    val pattern3 = "(ab|cd|pq|xy)".r
    time {println(filterLines(filterLines(filterLines(listOfLines,pattern1,">",2),pattern2,">",0),pattern3, "==", 0).length)}

    //part 2
    //contains any two letters that appears at least twice in the string without overlapping
    val pattern4 = "([a-zA-Z]{2}).*\\1".r
    //contains at least one letter which repeats with exactly one letter between them
    val pattern5 = "([a-zA-Z]).{1}\\1".r
    time {println(filterLines(filterLines(listOfLines, pattern5, ">", 0),pattern4, ">", 0).length)}
  }
}
