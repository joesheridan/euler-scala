import scala.io.Source

/*
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values
we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10.

If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand
common English words, how many are triangle words?
 */

def genTriangleNumbers(max: Int) : List[Int] = {
  (1 to max).map(a => a * 0.5 * (a + 1)).map(_.toInt).toList
}

val nums = genTriangleNumbers(100)

def getWordValue(s: String): Int = {
  s.foldLeft(0)((acc, c) => c.toInt - 64 + acc)
}

getWordValue("a")
getWordValue("b")
getWordValue("S")
getWordValue("SKY")

def testWord(s: String): Boolean = {
  nums.contains(getWordValue(s))
}

testWord("SKY")
testWord("JESSICA")
val fileContents = Source.fromFile("p022_names.txt").getLines.mkString
val names = fileContents.split(",").filter(testWord).length