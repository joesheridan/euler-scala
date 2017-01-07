package lattice

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent._
import akka.util.Timeout
import scala.util.{Success, Failure}
  
  /**
   * Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
   * there are exactly 6 routes to the bottom right corner.
   * How many such routes are there through a 20×20 grid?
   */
  
  object LatticePaths extends App {
    
    /**
     * the recursive method works but takes a long
     * time for grids above 15x15
     */
    def getPathsNumRecursive(x: Int, y: Int, limit: Int) : Int = {
      if (x == limit) return 1
      if (y == limit) return 1
      return getPathsNumRecursive(x, y + 1, limit) + getPathsNumRecursive(x + 1, y, limit)
    }
      
    /**
     * for a given range (params start and finish), calculate the number of valid
     * paths through the lattice
     */
    def getPathsNum(start:Long, finish:Long, gridSize:Int) : Long = {
      println("getpathsnum called- start:"+ start + " end:"+ finish)
      def isValidPath(path: Long, gridSize: Int) : Boolean = {
        // count the set bits in the proposed path
        // a valid path has an equal amount of 1s and 0s for a valid path
        (java.lang.Long.bitCount(path) == gridSize)
      }
      
      // count the valid paths
      var validPaths:Long = 0
      for (i <- (start to finish)) {
        if (isValidPath(i, gridSize))
        validPaths += 1
      }
      validPaths
    }
    
    /**
     * get the max possible value of a path for a given gridSize
     */
    def getMaxPath(gridSize: Int) : Long = {
      // create a string of 1's
      val maxbin = "1" * (gridSize * 2)
      return BigInt(maxbin,2).toLong // convert maxbin to a Long
    }
    
    /**
     * set up the future worker list
     */
    def getFutureResultsList() : List[Future[Long]] = {
      // create some worker threads via futures
      println("creating future workers")
      val gridSize = 15
      val numWorkers = 6
      var futures: List[Future[Long]] = List()
    
      // divide the work into chunks
      for (i <- 0 until numWorkers) {
        val chunkSize = getMaxPath(gridSize)/numWorkers
        val start = chunkSize * i
        var end = start + chunkSize
        if (i == numWorkers-1) end = getMaxPath(gridSize)
        val f = Future {
          getPathsNum(start,end,gridSize)
        }
        futures = futures :+ f
      }
      futures
    }
    
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
  }
