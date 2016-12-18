
/**
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * a2 + b2 = c2
 * For example, 32 + 42 = 9 + 16 = 25 = 52.
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */
object SpecialPythagoreanTriplet extends App {
  
  // determine whether the specified tuple is a pythagorean triplet
  def isPTriplet(tuple: Tuple3[Int, Int, Int]) : Boolean = {
    val (a,b,c) = tuple
    if (!(a < b) || !(b < c)) {
      println("a<b<c not true " + tuple.toString)
      return false
    }
    
    return (a*a + b*b == c*c)
  }
 
  // get the next sequential triplet where a+b+c = 1000
  def getNextTriplet(tuple: Tuple3[Int, Int, Int]) : (Int, Int, Int) = {
    val (a,b,c) = tuple
    if (a+b == 999) {
      return (a+1, a+2, 1000 - ((a + 1) + (a + 2)))
    } else {
      return (a, b+1, 1000 - (a + b + 1))
    }
  }
  
  var currentTriplet = (1,2,997)
  while (!isPTriplet(currentTriplet)) {
    currentTriplet = getNextTriplet(currentTriplet)
  }
  
  println("found: "+ currentTriplet.toString)
  
}