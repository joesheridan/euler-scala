
/**
 * calculate the sum of all fibonacci numbers under 4M which are even
 */

println("Even Fibonaccis")

// initialise list with 1 and 2
var fiblist = List(1,2)

def getNextFib(l: List[Int]):List[Int] = {
	val x = l.size
	l :+ l(x-1) + l(x-2)
}

// keep building the fib list until we reach 4M, then filter by evens and sum
Stream.iterate(fiblist)(getNextFib(_)).dropWhile(_.last < 4000000).head.filter(_ % 2 == 0).sum
