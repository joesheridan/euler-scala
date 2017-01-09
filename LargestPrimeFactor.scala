
/**
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 */

object LargestPrimeFactor extends App {
  
  def getPrimeFactors(num: Long) : List[Long] = {
    getFactors(num) filter (isPrime)
  }
  
  def isPrime(num: Long) : Boolean = {
    getFactors(num).length == 0
  }
  
  def getFactors(num: Long) : List[Long] = {
    var factors: List[Long] = List()
    var i = 2L
    while (i < num) {
      if (num % i == 0) {
        factors = factors :+ i
      }
      i += 1
    }
    factors
  }
  
  println(getPrimeFactors(600851475143L))
}