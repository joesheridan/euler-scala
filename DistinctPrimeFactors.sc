/*
The first two consecutive numbers to have two distinct prime
factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime
factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct
prime factors each. What is the first of these numbers?
 */

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.{Success, Failure}

def isFactor(a: Int, b: Int): Boolean = a % b == 0

def isPrime(num: Int) : Boolean = {
  // make sure that every number less than num but greater than 1 is not a factor
  val range = (2 to (num-1)).view
  return !range.exists(i => (num % i) == 0)
}

def getPrimeFactors(n: Int) : List[Int] = {
  lazy val factors = (2 to n/2).view.filter(isFactor(n, _))
  lazy val primes = factors.filter(isPrime(_))
  primes.toList
}

def hasConsecutivePrimeFactors(consecutive: Int, numFactors:Int, n: Int) : Boolean = {
  lazy val numPrimeFactors = (0 until consecutive).view.
    map(a => getPrimeFactors(n + a)).
    map(_.length)
  //println(numPrimeFactors)
  numPrimeFactors.forall(_ == numFactors)
}

getPrimeFactors(644)
hasConsecutivePrimeFactors(3,3,644)

//Stream.iterate(1)(_ + 1).filter(hasConsecutivePrimeFactors(4, 4, _)).head
//(1 to 104400).filter(hasConsecutivePrimeFactors(4, 4, _)).toList
//val l = List(1308, 1884, 2013, 2583, 2664, 2665, 2714, 2715, 2793, 3618, 3729, 3912, 3913, 4233, 4254, 4344, 4653, 5073, 5133, 5289, 5450, 5451, 5733, 6094, 6305, 6354, 6355, 6363)
//l.filter(hasConsecutivePrimeFactors(4, 4, _))

def splitComputation(limit: Int, partitions:Int): List[Future[List[Int]]] = {
  val partitionSize = limit / partitions
  val known = 100000
  val futures = for (i <- 0 to partitions) yield {
    val start = partitionSize * i + known
    val finish = start + partitionSize
    Future { (start to finish).view.filter(hasConsecutivePrimeFactors(4, 4, _)).take(1).toList }
  }
  futures.toList
}


val resultsList = Future.sequence(splitComputation(1000000, 8))
resultsList.onComplete {
  case Success(results) => {
    println("results list:"+results)
  }
  case Failure(t) => println("failed.."+t.getMessage)
}

// keep the main app alive until the results come back
while(!resultsList.isCompleted) {
  Thread.sleep(100)
}

// 134043