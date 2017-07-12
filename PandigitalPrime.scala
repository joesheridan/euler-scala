/**
  * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
  * For example, 2143 is a 4-digit pandigital and is also prime.
  *
  * What is the largest n-digit pandigital prime that exists?
  */

def isPrime(num: Int) : Boolean = {
  if (num % 2 == 0)
    return false
  !(2 to (num-1)).exists(i => (num % i) == 0) && num > 1
}

def getLargestPanPrime(n:Int) : Int = {
  // reverse the list so that the largest number appears first
  // then take the first prime which will also be the highest
  (1 to n).toList.reverse.permutations.map(_.mkString.toInt).find(isPrime(_)) match {
    case None => 0
    case Some(i: Int) => i
  }
}

val primes = for (i <- 4 to 9) yield {
  val a = getLargestPanPrime(i)
  println("i:" + i + " largest prime:" + a)
  a
}
primes.max