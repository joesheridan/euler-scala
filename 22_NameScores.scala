
/**
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over
 * five-thousand first names, begin by sorting it into alphabetical order.
 * Then working out the alphabetical value for each name, multiply this value by its alphabetical
 * position in the list to obtain a name score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN, which is worth
 * 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
 * What is the total of all the name scores in the file?
 */
package namescores
import scala.io.Source

/**
  * get the score for an individual char
  * if char is in the right range, subtract 64 (from ASCII val) to
  * get the letter index
  */
def getCharScore(c: Char): Int = {
  val cint = c.toInt
  if (cint < 64 || cint > 90) return 0
  cint - 64
}

/**
 * get the overall score for the given name param
 */
def getScore(name: String, index: Int): Int = {
  val score = name.map(getCharScore).sum * index
  println(name + " " + score)
  score
}

val fileContents = Source.fromFile("p022_names.txt").getLines.mkString
val names = fileContents.split(",")

//add all the scores up
names.zipWithIndex.foldLeft(0) {
  case (t, (name, ind) ) => t + getScore (name, ind + 1)
}

