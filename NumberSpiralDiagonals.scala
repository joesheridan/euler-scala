/**
Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
*/


def getSpiralLength(depth: Int): Int = {
  if (depth == 1)
    return 1
  val sideLength = (2 * depth) - 1
  val total = sideLength * 2 + (sideLength - 2) * 2 + getSpiralLength(depth - 1)
  total
}

def getDiagonalsSum(n: Int): Int = {
  if (n == 1)
    return 1
  val sideLen = (2 * n) - 1
  val stotal = getSpiralLength(n - 1)
  val br = stotal + sideLen - 1
  val bl = br + sideLen - 1
  val tl = bl + sideLen - 1
  val tr = tl + sideLen - 1
  //println("tr:" + tr + " tl:" + tl + " bl:" + bl + " br:" + br)
  tr + tl + bl + br + getDiagonalsSum(n - 1)
}

getDiagonalsSum(1)
getDiagonalsSum(3)
getDiagonalsSum(1001)

