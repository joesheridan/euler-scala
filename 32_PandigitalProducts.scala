/**
  * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example,
  * the 5-digit number, 15234, is 1 through 5 pandigital.
  *
  * The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1
  * through 9 pandigital.
  *
  * Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
  *
  * HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
  */
case class MMP(multiplicand: Int, multiplier: Int, product: Int) {
  def isPandigital9: Boolean = {
    val allstr = multiplicand.toString + multiplier.toString + product.toString
    allstr.length == 9 && (1 to 9).forall(i => allstr.contains(i.toString)) && !allstr.contains("0")
  }
}

MMP(39, 186, 7254).isPandigital9

val max = 10000
val tuples = for (i <- (1 to max).iterator; j <- (1 to max).iterator) yield MMP(i, j, i * j)

val products = tuples.filter(_.isPandigital9).map(_.product).toList.distinct
products.sum