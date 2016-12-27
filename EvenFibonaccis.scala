
/**
 * calculate the sum of all fibonacci numbers under 4M which are even
 */
object scala extends App {
  println("Even Fibonaccis")

	// initialise list with 1 and 2
	var mylist = List(1,2);

	def getNextFib(l: List[Int]):Int = {
		val x = l.size
		return l(x-1) + l(x-2)
	}

	// generate list of fibonaccis with values < 4 million
	while (getNextFib(mylist) <= 4000000) {
		val f = getNextFib(mylist)
		mylist = mylist :+ f
	}

	println("all fibs < 4M" + mylist)

	// add them up
	println("result:" + mylist.filter(_ % 2 == 0).sum)

}
