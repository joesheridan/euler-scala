
/**
 * calculate the sum of all fibonacci numbers under 4M which are even
 */
object fibonnaccis extends App {
  println("Even Fibonaccis")

	// initialise list with 1 and 2
	var fiblist = List(1,2);

	def getNextFib(l: List[Int]):Int = {
		val x = l.size
		return l(x-1) + l(x-2)
	}

	// generate list of fibonaccis with values < 4 million
	while (getNextFib(fiblist) <= 4000000) {
		fiblist = fiblist :+ getNextFib(fiblist)
	}

	println("all fibs < 4M" + fiblist)

	// filter for even numbers and add them up 
	println("result:" + fiblist.filter(_ % 2 == 0).sum)

}
