/**
 * Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
 * there are exactly 6 routes to the bottom right corner.
 * How many such routes are there through a 20×20 grid?
 */
package lattice

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.{Success, Failure}

/**
 * Path represents a single path through the grid using a binary
 * number system to represent left/down choices
 */
case class Path(path: Long, gridSize: Int) {
  def isValid() : Boolean = {
      // count the set bits in the proposed path
      // a valid path has an equal amount of 1s and 0s for a valid path
      (java.lang.Long.bitCount(path) == gridSize)
    }
}


/**
 * Represents a chunk of work
 */
case class Chunk(index: Int, numWorkers: Int, maxPath: Long) {
  def chunkSize = (maxPath/numWorkers)
  def getStart = chunkSize * index
  def getFinish = {
    var end = getStart + chunkSize
    if (index == numWorkers-1) end = maxPath
    end
  }
}
  
/**
 * Calculates the number of paths through a grid
 */

  
/**
 * the recursive method works but takes a long
 * time for grids above 15x15
 */
def getPathsNumRecursive(x: Int, y: Int, limit: Int) : Int = {
  if (x == limit || y == limit) return 1
  return getPathsNumRecursive(x, y + 1, limit) + getPathsNumRecursive(x + 1, y, limit)
}

def isValidPath(path: Long, gridSize:Int): Boolean = {
java.lang.Long.bitCount(path) == gridSize
}

/**
 * for a given range (chunk param), calculate the number of valid
 * paths through the lattice
 */
def getPathsNum(chunk: Chunk, gridSize:Int) : Long = {
  println("getpathsnum called- start:"+ chunk.getStart + " end:"+ chunk.getFinish)

  Stream.iterate(chunk.getStart)(_ + 1).
    takeWhile(_ <= chunk.getFinish).
    filter(isValidPath(_, gridSize)).
    length
}

/**
 * get the max possible value of a path for a given gridSize
 */
def getMaxPath(gridSize: Int) : Long = {
  // create a string of 1's
  val maxbin = "1" * (gridSize * 2)
  BigInt(maxbin,2).toLong // convert maxbin to a Long
}

/**
 * set up the future worker list
 */
def getFutureResultsList() : List[Future[Long]] = {
  // create some worker threads via futures
  println("creating future workers")
  val gridSize = 15
  val numWorkers = 6

  // divide the work into chunks
  val futures = for (i <- 0 until numWorkers)
    yield (Future { getPathsNum(Chunk(i, numWorkers, getMaxPath(gridSize)), gridSize) })

  futures.toList
}

// wait for the results to complete..
val resultsList = Future.sequence(getFutureResultsList())
resultsList.onComplete {
  case Success(results) => {
    println("results list:"+results)
    println("results:"+results.sum)
  }
  case Failure(t) => println("failed.."+t.getMessage)
}

// keep the main app alive until the results come back
while(!resultsList.isCompleted) {
   Thread.sleep(100)
}

