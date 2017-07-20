
/**
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 * What is the 10 001st prime number?
 */


  
  def isPrime(num: Int) : Boolean = {
    // make sure that every number less than num but greater than 1 is not a factor
    val range = 2 to (num / 2)
    return !range.exists(i => (num % i) == 0) 
  }
  
//  var primes = 0
//  var i = 2
//  while (primes < 10001) {
//    if (isPrime(i)) {
//      primes += 1
//      println("prime number" + primes + " found:" + i)
//    }
//    i += 1
//  }


(2 to 1000000).view.filter(isPrime(_)).zipWithIndex.dropWhile{ case (p, ind) => ind <= 10000 }.head
  
