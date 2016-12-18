
/*The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*/

object SumSquareDifference extends App {
  
    def SumOfSquares(num: Int): Int = {
      if (num == 1) return 1
      return (num*num) + SumOfSquares(num-1)
    }
    
    def SquareOfSum(num: Int): Int = {
      val range = 1 to num
      return range.sum * range.sum
    }
  
    println(SquareOfSum(100) - SumOfSquares(100))
}