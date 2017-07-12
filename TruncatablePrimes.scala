
/**
  * The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from
  * left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  */

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

def isPrime(num: Int) : Boolean = {
  val range = 2 to (num-1)
  return !range.exists(i => (num % i) == 0) && num > 1
}

def getTruncated(num: Int, left: Boolean): List[Int] = {
  val numstr = num.toString
  var current = numstr
  var results: List[Int] = List(current.toInt)
  for (i <- 1 until numstr.length) yield {
    if (left)
      current = current.drop(1)
    else
      current = current.dropRight(1)

    results = results :+ current.toInt
  }
  results
}

def isTruncatablePrime(num: Int): Boolean = {
  val nums = (getTruncated(num, true) ++ getTruncated(num, false)).distinct
  nums.forall(isPrime(_)) && !Array(2,3,5,7).contains(num)
}

getTruncated(7379, true)
getTruncated(7379, false)

isTruncatablePrime(3797)
isTruncatablePrime(3798)

def splitRange(r: Range, chunks: Int): Seq[Range] = {
  if (r.step != 1)
    throw new IllegalArgumentException("Range must have step size equal to 1")

  val nchunks = scala.math.max(chunks, 1)
  val chunkSize = scala.math.max(r.length / nchunks, 1)
  val starts = r.by(chunkSize).take(nchunks)
  val ends = starts.map(_ - 1).drop(1) :+ r.end
  starts.zip(ends).map(x => x._1 to x._2)
}

val max = 1000000
val chunks = 8
val futuresList = splitRange(1 until max, chunks).map { r =>
  Future {
    r.filter(isTruncatablePrime(_)).toList
  }
}

val seqFutures = Future.sequence(futuresList)

seqFutures.onComplete {
  case Success(results) => {
    println("results list:"+results.toList.flatten + " count:" + results.toList.flatten.length + " sum:" + results.toList.flatten.sum)
  }
  case Failure(t) => println("failed.."+t.getMessage)
}

// keep the main app alive until the results come back
while(!seqFutures.isCompleted) {
  Thread.sleep(100)
}