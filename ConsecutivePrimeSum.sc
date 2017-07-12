/*
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
 */

def isPrime(num: Int) : Boolean = {
  val range = 2 to (num-1)
  return !range.exists(i => (num % i) == 0) && num > 1
}

val primes = (2 to 1000).filter(isPrime(_))

def getConsecutivePrimeSum(primes: List[Int], a:Int): Int = {
    primes(a) + primes(a+1)
}

primes.zipWithIndex.map { case (a, index) => a -> getConsecutivePrimeSum(primes, index) }