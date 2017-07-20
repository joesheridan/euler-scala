/*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330,
is unusual in two ways: (i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
 */

def isPrime(num: Int) : Boolean = {
  // make sure that every number less than num but greater than 1 is not a factor
  val range = 2 to (num-1)
  return !range.exists(i => (num % i) == 0)
}

def getPrimePermutations(n:Int): List[Int] = {
  if (n.toString.contains("0"))
    return Nil
  n.toString.permutations.map(_.toInt).filter(isPrime(_)).toList
}

def getPrimeTriples(nums: List[Int]) = {
  //nums.permutations.toList
  for (i <- nums; j <- nums; k <- nums if (i < j && j < k)) yield (i, j, k)
}

def isSequence(l: (Int, Int, Int)): Boolean = {
  val diff1 = l._2 - l._1
  val diff2 = l._3 - l._2
  diff1 == diff2
}

getPrimeTriples(getPrimePermutations(3709))
isSequence((1487, 4817, 8147))

val results = (1487 to 9488).view.filter(isPrime(_)).
  map(getPrimePermutations(_)).
  map(getPrimeTriples(_)).flatten.filter(isSequence(_)).toList.distinct

results.map{ case (a,b,c) => a.toString + b.toString + c.toString }

