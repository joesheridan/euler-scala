/*
The number, 1406357289, is a 0 to 9 pandigital number because
it is made up of each of the digits 0 to 9 in some order, but
it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this
way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
 */

def isPandigital(num: BigInt): Boolean = {
  val allstr = num.toString
  allstr.length == 9 && (0 to 9).forall(i => allstr.contains(i.toString))
}

def hasSubStringDivisibility(num: List[Int]): Boolean = {
  val numstr = num.mkString
  val r1 = numstr.substring(1, 1 + 3).toInt % 2 == 0
  val r2 = numstr.substring(2, 2 + 3).toInt % 3 == 0
  val r3 = numstr.substring(3, 3 + 3).toInt % 5 == 0
  val r4 = numstr.substring(4, 4 + 3).toInt % 7 == 0
  val r5 = numstr.substring(5, 5 + 3).toInt % 11 == 0
  val r6 = numstr.substring(6, 6 + 3).toInt % 13 == 0
  val r7 = numstr.substring(7, 7 + 3).toInt % 17 == 0
  r1 && r2 && r3 && r4 && r5 && r6 && r7
}
//hasSubStringDivisibility(1406357289)

//hasSubStringDivisibility(1406357287)

List(0,1,2,3,4,5,6,7,8,9).permutations.
  filter(hasSubStringDivisibility(_)).toList.
  map(_.mkString)