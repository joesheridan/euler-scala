/*
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
 */

def isPrime(num: Int) : Boolean = {
  if (num < 2)
    return false
  val range = (2 to (num / 2)).view
  return !range.exists(i => (num % i) == 0) && num > 1
}

val max = 1000000
// the longest chain will be within the first 1000 primes
val primes = (2 to max).view.filter(isPrime(_)).take(1000).force

/**
  * get the list of valid consecutive prime chains from a given start index
  * @param primes - the precomputed list of prime numbers
  * @param start - the start index from which to start the chains
  * @param max - the max number to restrict valid entrys to
  * @return - a sequence of 3-tuples containing the start prime, the sum, and the number of primes in the chain
  */
def getConsecutivePrimeSums(primes: Seq[Int], start:Int, max: Int): Seq[(Int, Int, Int)] = {

  primes.drop(start).scanLeft(0)(_ + _).drop(1).zipWithIndex.collect {
    case (x, ind) if x <= max && ind > 0 && isPrime(x)  => (primes(start), x, ind + 1)
  }
}

val results = primes.zipWithIndex.map {
  case(p, i) => getConsecutivePrimeSums(primes, i, max)
}

val mostConsecutive = results.flatten.maxBy(_._3)