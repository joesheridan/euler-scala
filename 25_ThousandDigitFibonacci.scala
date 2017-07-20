
/**
 * The Fibonacci sequence is defined by the recurrence relation:
 * 
 * Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
 * Hence the first 12 terms will be:
 * 
 * F1 = 1
 * F2 = 1
 * F3 = 2
 * F4 = 3
 * F5 = 5
 * F6 = 8
 * F7 = 13
 * F8 = 21
 * F9 = 34
 * F10 = 55
 * F11 = 89
 * F12 = 144
 * The 12th term, F12, is the first term to contain three digits.
 * 
 * What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
 */

/**
  * maintain a list of the last 2 fibonnacis, so the next can be computed
  */
def getNextFib(fibList: List[BigInt]): List[BigInt] = {
  val len = fibList.length
  val next = fibList.takeRight(2).sum
  List(fibList(len - 1), next)
}

Stream.iterate(List(BigInt(1),BigInt(2)))(getNextFib(_)).dropWhile(_.last.toString.length < 1000).take(1).head.last
