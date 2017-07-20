/*
A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
 its decimal fraction part.
 */

// determine whether the repeating group is a single number
// e.g. 1.3333333333333
def singleRepeat(bignum: String) : Boolean = {
  val first = bignum.toList.drop(3).head
  bignum.toList.drop(3).forall(_ == first)
}


// create a repeated string from the group and compare it to the original string
def repeats(group: List[Char], bignum: String): Boolean = {
  val repeated = group.mkString * bignum.length take(bignum.length)
  //println(repeated)
  repeated == bignum
}

def getTestableNums(num: String) : List[String] = {
  List(num, num.drop(1), num.drop(2), num.drop(3))
}

def getRepeatGroup(num: String) : List[Char] = {
  val a: List[Char] = List()

  for (i <- 1 to (num.length / 2);
      nums <- getTestableNums(num)) yield {
    if (repeats(nums.toList.take(i), nums)) {
      //println("group:" + num.toList.take(i) + " repeats:" +)
      return num.toList.take(i)
    }
  }
  return a
}

// build the fractions list
val fractions = (1 to 1000).toList map (d =>
  (d, new java.math.BigDecimal(1).divide(new java.math.BigDecimal(d.toString()), 1200, java.math.RoundingMode.CEILING)))

// find the max repeat group for each decimal fraction
val repeatgroups = fractions.filter(a => {
    val len = a._2.toString().drop(2).length
    len > 30
  }).
  map{ case (d, num) => {
    val digits = num.toString().drop(2).dropRight(1)
    (d, digits, getRepeatGroup(digits))
  }}.
  map { case (d,fraction, repeatgroup) => (d, fraction, repeatgroup.length) }

println(repeatgroups)

println("max repeat group:" + repeatgroups.maxBy(_._3))





























