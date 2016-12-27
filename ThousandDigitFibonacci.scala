
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
object ThousandDigitFibonacci extends App {
  
  var fibs:List[BigInt] = List(1,1)
  var i = 1
  while(getDigits(getNextFib()) < 1000) {
    i += 1
    println("last:"+fibs.last)
  }
  
  /**
   * get the next fibonacci in the sequence and add
   * to the fibs list
   */
  def getNextFib():BigInt = {
    val next = fibs.takeRight(2).sum
    // save the new list
    fibs = fibs ::: List(next)
    next
  }
  
  /**
   * recursive fibonacci method works but is slow
   */
  def getNextFibRecursive(num:BigInt):BigInt = {
    if (num == 1 || num == 2) return 1
    getNextFibRecursive(num-1) + getNextFibRecursive(num-2)
  }
  
  /**
   * get the number of digits for the given num param
   * e.g. 1000 is 4 digits
   */
  def getDigits(num:BigInt):Int = {
    num.toString.length
  }
}
