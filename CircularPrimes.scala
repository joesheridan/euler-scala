/**

The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?

  */

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

def rotate(num: List[Int]): List[Int] = {
  num.tail :+ num.head
}

def getRotations(num: Int): List[Int] = {
  var current = num.toString.map(_.asDigit).toList
  var results = List(current.mkString.toInt)

  for (i <- 1 until current.length) yield {
    current = rotate(current)
    results = results :+ current.mkString.toInt
  }
  results
}

def isPrime(num:Int): Boolean = {
  if (num == 1) return false
  val range = 2 to num-1
  val hasFactor = range.exists(num % _ == 0)
  !hasFactor
}

def isCircular(num: Int): Boolean = {
  getRotations(num).forall(isPrime(_))

}

getRotations(197)
isCircular(197)
isCircular(198)

def splitRange(r: Range, chunks: Int): Seq[Range] = {
  if (r.step != 1)
    throw new IllegalArgumentException("Range must have step size equal to 1")

  val nchunks = scala.math.max(chunks, 1)
  val chunkSize = scala.math.max(r.length / nchunks, 1)
  val starts = r.by(chunkSize).take(nchunks)
  val ends = starts.map(_ - 1).drop(1) :+ r.end
  starts.zip(ends).map(x => x._1 to x._2)
}

val futuresList = splitRange(2 until 100, 8).map { r =>
  Future {
    r.filter(isCircular(_)).toList
  }
}
val seqFutures = Future.sequence(futuresList)
seqFutures.onComplete {
  case Success(results) => {
    println("results list:"+results.toList.flatten + " count:" + results.toList.flatten.length)
  }
  case Failure(t) => println("failed.."+t.getMessage)
}

// keep the main app alive until the results come back
while(!seqFutures.isCompleted) {
  Thread.sleep(100)
}