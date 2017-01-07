
/**
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called 
 * amicable numbers.
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
 * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 * Evaluate the sum of all the amicable numbers under 10000.
 */

object AmicableNumbers extends App {
  
  /**
   * Get sum of list of divisors of param n
   */
  def d(n:Int):Int = {
    (1 to n-1).filter(n % _ == 0).sum
  }
  
  /**
   * return a list of amicable pair numbers less than param max
   */
  def getAmicablePairs(max:Int) : List[Int] = {
    val pairs = for (i <- 1 to max if (i == d(d(i)) && d(i) != d(d(i)))) 
                    yield List(d(i), d(d(i)))
    println(pairs.toList.flatten)
    pairs.toList.flatten
  }
  
  val amicables = getAmicablePairs(10000);
  println("sum of amicable pairs:"+amicables.distinct.sum)
  
}