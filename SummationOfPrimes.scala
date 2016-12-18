/**
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 */

object SummationOfPrimes extends App {
  
  def isPrime(num:Int): Boolean = {
    if (num == 1) return false
    val range = 2 to num-1
    val hasFactor = range.exists(num % _ == 0)
    if (!hasFactor) println(num)
    !hasFactor
  }
  val range = 1 to 2000000 
  val primes = range.filter(isPrime(_))
  println(primes)
  println(primes.reduceLeft(_+_))
}