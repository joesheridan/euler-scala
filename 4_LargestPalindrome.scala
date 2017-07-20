
/**
 * find largest palindrome less than 999 * 999
 */


/**
 * find the first palindrome less than 999 * 999
 */
def findPalins(): Int = {
	Stream.iterate(999 * 999)(_ - 1).filter(isPalindromic(_)).head
}

/**
 * Determine whether param i is palindromic
 */
def isPalindromic (i : Int) : Boolean = {
	val len = i.toString().length
	val iList = i.toString().toList

	// split the number into two halves (left and right) and
	// if the two halves are the same,
	// the number is palindromic
	val left = iList.take(Math.floor(len / 2).toInt)
	val right = iList.takeRight(Math.floor(len / 2).toInt).reverse

	left == right
}


findPalins()