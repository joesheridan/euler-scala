
/**
 * find largest palindrome less than 999 * 999
 */
object LargestPalindrome extends App {
	
  findPalins()
	
  def findPalins() {
		for (i <- (999 * 999) to 1 by -1) {
			if (isPalindromic(i)) {
				println("found palindrome: " + i)
				return
			}
		}
	}
	def isPalindromic (i : Int) : Boolean = {
		val len = i.toString().length
		var iList = i.toString().toList

		// halve the number

		val left = iList.take(Math.floor(len / 2).toInt)
		val right = iList.takeRight(Math.floor(len / 2).toInt).reverse

		return (left == right)
	}

}