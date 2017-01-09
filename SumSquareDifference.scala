
/**
 * The sum of the squares of the first ten natural numbers is,
 * 1^2 + 2^2 + ... + 10^2 = 385
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)2 = 55^2 = 3025
 * Hence the difference between the sum of the squares of the first ten 
 * natural numbers and the square of the sum is 3025 − 385 = 2640.
 * Find the difference between the sum of the squares of the first one 
 * hundred natural numbers and the square of the sum.
 */
object SumSquareDifference extends App {
  
  /**
   * returns the sum of squares of ints up to param max
   */
  def sumOfSquares(max:Int): Int = {
    val squares = (1 to max) map { x => x * x }
    squares.sum
  }
  
  /**
   * returns the square of the sum of ints up to param max
   */
  def squareOfSum(max:Int): Int = {
    val sum = (1 to max).sum
    sum * sum
  }
  
  println("sumofsquares:"+sumOfSquares(100) + " squareofsum:"+squareOfSum(100))
  println("difference:"+(squareOfSum(100) - sumOfSquares(100)))
  
}