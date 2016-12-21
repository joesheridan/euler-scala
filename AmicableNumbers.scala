
/**
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called 
 * amicable numbers.
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
 * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 * Evaluate the sum of all the amicable numbers under 10000.
 */
object AmicableNumbers extends App {
  def d(n:Int):Int = {
    (1 to n-1).filter(n % _ == 0).sum
  }
  def getAmicablePairs(max:Int) : List[Int] = {
    var alist:List[Int] = List()
    for (i <- 1 to max) {
      if (i == d(d(i)) && d(i) != d(d(i))) {
        alist = alist ::: List(d(i), d(d(i)))
      }
    }
    alist
  }
  
  val amicables = getAmicablePairs(10000);
  amicables.distinct.foreach(println)
  println("sum of amicable pairs:"+amicables.distinct.sum)
  
}