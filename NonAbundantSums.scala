
/**
 * A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
 * For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 
 * is a perfect number.
 * A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant 
 * if this sum exceeds n.
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as 
 * the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater 
 * than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced 
 * any further by analysis even though it is known that the greatest number that cannot be expressed as the 
 * sum of two abundant numbers is less than this limit.
 * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
object NonAbundantSums extends App {
  
  val limit = 28123
  val abundantNums = getAbundantNumbers(limit)
  println("abundant numbers:"+abundantNums.takeRight(10))
  val abundantSums = getAbundantSums(abundantNums).sorted
  println("abundant sums:"+abundantSums.takeRight(10))
  val nonAbundantSums = getInverse(abundantSums, limit)
  println("non abundant sums:"+nonAbundantSums.sum)
  
  /**
   * get the divisors of param num
   */
  def getDivisors(num:Int) : List[Int] = {
    (1 to num-1).filter(num % _ == 0).toList
  }
  
  /**
   * determine whether param num is abundant
   */
  def isAbundant(num:Int) : Boolean = {
    val divisors = getDivisors(num).sum
    (divisors > num)
  }
  
  /**
   * get all the abundant numbers up to the limit param
   */
  def getAbundantNumbers(limit:Int) : List[Int] = {
    (1 to limit).filter(isAbundant).toList
  }
  
  /**
   * for the given abundant number (num param),
   * get all the possible sums against the other abundant numbers 
   * within the known limit
   */
  def getSums(num:Int, abundants:List[Int]): List[Int] = {
    abundants.map{_+num}.filter(_ <= 28123)
  }
  
  /**
   * from a list of sums of two abundant numbers,
   * get the integers which are not listed (the inverse)
   */
  def getInverse(nums:List[Int], limit:Int):List[Int] = {
    (1 to limit).filter { a => !nums.contains(a) }.toList
  }
  
  /**
   * get all the numbers which are sums of two abundant numbers
   */
  def getAbundantSums(abundants:List[Int]): List[Int] = {
    // converting from list to array massively improves performance for this step
    var abundantsArr = abundants.toArray
    val results = abundantsArr.map (getSums(_, abundants)).flatten
    results.distinct.toList
  }
  
}