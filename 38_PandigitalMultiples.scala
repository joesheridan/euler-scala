/**
  * Take the number 192 and multiply it by each of 1, 2, and 3:

  192 × 1 = 192
  192 × 2 = 384
  192 × 3 = 576
  By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the
  concatenated product of 192 and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital,
  918273645, which is the concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an
  integer with (1,2, ... , n) where n > 1?
  */


def isPandigital(num: BigInt): Boolean = {
  val allstr = num.toString
  allstr.length == 9 && (1 to 9).forall(i => allstr.contains(i.toString)) && !allstr.contains("0")
}

def getProduct(base: Int, nums: List[Int]): BigInt = {
  BigInt(nums.foldLeft("")((acc, a) => acc + (a * base).toString))
}

getProduct(9, List(1,2,3))

class GenConcatenatedProducts extends Iterator[(Int, List[Int])] {
  var cur = 1
  var base = 1
  def next = {
    cur = cur + 1
    // increment the base if the product is greater than 9 digits
    if (getProduct(base, (1 to cur).toList).toString.length > 9) {
      cur = 2
      base = base + 1
    }
    (base, (1 to cur).toList)
  }
  // stop when base times the base list is greater than 9 characters
  def hasNext = (getProduct(base, List(1,2)).toString.length <= 9)
}

val i = new GenConcatenatedProducts()
i.collect {
  case (base, l) if isPandigital(getProduct(base, l)) == true => {
    println((base, l.length, getProduct(base, l)))
    getProduct(base, l)
  }
}.toList.max
