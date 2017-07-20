
/**
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 */

def isPrime(num: Long): Boolean = {
  if (num == 1) return false
  val range = 2L to (num / 2L)
  val hasFactor = range.exists(num % _ == 0)
  !hasFactor
}

def getLargestFactor(num: Long): Long = {
  Stream.iterate(num / 2)(_ - 1).filter(num % _ == 0).filter(isPrime(_)).head
}

getLargestFactor(13195L)
getLargestFactor(600851475143L)
