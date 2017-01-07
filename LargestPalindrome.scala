
/**
 * find largest palindrome less than 999 * 999
 */
object LargestPalindrome extends App {
	
  findPalins()
	
  /**
   * find the first palindrome less than 999 * 999
   */
  def findPalins() {
		for (i <- (999 * 999) to 1 by -1) {
			if (isPalindromic(i)) {
				println("found palindrome: " + i)
				return
			}
		}
	}
  
  /**
   * Determine whether param i is palindromic
   */
	def isPalindromic (i : Int) : Boolean = {
		val len = i.toString().length
		var iList = i.toString().toList

		// split the number into two halves (left and right) and 
		// if the two halves are the same,
		// the number is palindromic
		val left = iList.take(Math.floor(len / 2).toInt)
		val right = iList.takeRight(Math.floor(len / 2).toInt).reverse

		return (left == right)
	}

}