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
val primes = (2 to max).view.filter(isPrime(_)).take(1000).force


def getConsecutivePrimeSums(primes: Seq[Int], start:Int, max: Int): Seq[(Int, Int, Int)] = {

  primes.drop(start).scanLeft(0)(_ + _).drop(1).zipWithIndex.collect {
    case (x, ind) if x <= max && ind > 0 && isPrime(x)  => (primes(start), x, ind + 1)
  }
}

val results = primes.zipWithIndex.map {
  case(p, i) => getConsecutivePrimeSums(primes, i, max)
}
val mostConsecutive = results.flatten.maxBy(_._3)