/**
 *  2520 is smallest number divisible by 1 to 10 no remainder
 *  
 *  find smallest int divisible by 1-20
 */

/**
 *  test the input number to see if its divisible by the divider
 */
def testMultiple(input: Int, divider: Int): Boolean = {
	var res = (input % divider);
	(res == 0)
}

/**
 *  test to see if num is divisible by all ints up to max
 */
def findMultiples(num: Int, max: Int): Boolean = {
	// count the factors
	val factors = (1 to max).filter(testMultiple(num, _))
	factors.length == max
}

Stream.iterate(1)(_ + 1).filter(findMultiples(_, 20)).head

