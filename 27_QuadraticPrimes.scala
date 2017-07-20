/**
  * Created by joe on 31/05/2017.
  */
/*
Considering quadratics of the form:

n2+an+bn2+an+b, where |a|<1000|a|<1000 and |b|≤1000

Find the product of the coefficients, aa and bb, for the quadratic expression that produces the maximum number of primes for consecutive values of nn, starting with n=0n=0.
*/

def isPrime(num: Int) : Boolean = {
  val absnum = scala.math.abs(num)
  // make sure that every number less than num but greater than 1 is not a factor
  val range = 2 to (absnum-1)
  //println("num:" + num + " isprime: " + !range.exists(i => (absnum % i) == 0))
  return !range.exists(i => (absnum % i) == 0)
}

def genProduct(n: Int, a: Int, b: Int): Int = {
  //println("gen product:" + ((n*n) + n*a + b))
  ((n*n) + n*a + b)
}

def getConsecutivePrimes(a: Int, b: Int) : Int = {
  var n = 0
  while (isPrime(genProduct(n, a, b))) {
    n = n + 1
  }
  n
}
var results: List[(Int, Int, Int)] = List()
for (a <- -999 to 999; b <- -1000 to 1000) yield {
  val consecPrimes = getConsecutivePrimes(a, b)

  if (consecPrimes > 10) {
    println("consecutive primes a:" + a + " b:" + b + " consec:" + consecPrimes)
    results = results :+ (a, b, consecPrimes)
  }
}

println("max num of consecutive primes:" + results.maxBy(_._3))


