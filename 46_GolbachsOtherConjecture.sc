/*
It was proposed by Christian Goldbach that every odd composite
number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the
sum of a prime and twice a square?
 */

def isPrime(num: Int) : Boolean = {
  // make sure that every number less than num but greater than 1 is not a factor
  val range = 2 to (num-1)
  return !range.exists(i => (num % i) == 0)
}

def isPrimeTwiceSqr(num: Int): Boolean = {
  val valids = (1 until num).filter(isPrime(_)).
    map(p => num - p).
    filter(isTwiceSquare(_)).toList
  valids.length > 0
}

// calculate a list of twice square sums to see if num matches one
def isTwiceSquare(num: Int) : Boolean = {
  Stream.iterate(1)(a => 2 + (a * 2)).takeWhile(_ <= num).
    contains(num)
}

isPrimeTwiceSqr(7)
isPrimeTwiceSqr(9)
isPrimeTwiceSqr(55) // false

val composites = Stream.iterate(9)(_ + 2).filter(!isPrime(_))
val firstNegative = composites.filter(!isPrimeTwiceSqr(_)).head

// 55