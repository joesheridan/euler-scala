/*
Considering quadratics of the form:

n2+an+bn2+an+b, where |a|<1000|a|<1000 and |b|≤1000

Find the product of the coefficients, aa and bb, for the quadratic expression that produces the maximum number of primes for consecutive values of nn, starting with n=0n=0.
*/

def isPrime(num: Int) : Boolean = {
  // make sure that every number less than num but greater than 1 is not a factor
  val range = 2 to (num-1)
  return !range.exists(i => (num % i) == 0)
}

def genProduct(n: Int, a: Int, b: Int): Int = (n*n) + n*a + b

def getConsecutivePrimes(a: Int, b: Int) : Int = {
//  var n = 0
//  while (isPrime(genProduct(n, a, b))) {
//    n = n + 1
//  }
//  n
  val results = Stream.iterate(0)(_ + 1).takeWhile(n => isPrime(genProduct(n, a, b))).toList
  if (results.length > 0)
    results.last
  else
    0
}

for (a <- -999 to 999; b <- -1000 to 1000) yield {
  println("consecutive primes a:" + a + " b:" + b + " consec:" + getConsecutivePrimes(a, b))
}

