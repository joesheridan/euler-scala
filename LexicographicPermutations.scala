
/**
 * A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of 
 * the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, 
 * we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
 * 
 * 012   021   102   120   201   210
 * 
 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 * 
 * List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
 * List(0, 1, 2, 3, 4, 5, 6, 7, 9, 8)
 * List(0, 1, 2, 3, 4, 5, 6, 9, 7, 8)
 * List(0, 1, 2, 3, 4, 5, 6, 9, 8, 7)
 * List(0, 1, 2, 3, 4, 5, 6, 7, 9, 8)
 */

object LexicographicPermutations extends App {

  // find the millionth permutation
  println("millionth permutation:"+getNthPermutation(List(0,1,2,3,4,5,6,7,8,9), 1000000))
  
  /**
   * get the nth lexicographic permutation in a list
   */
  def getNthPermutation(list:List[Int], n:Int): List[Int] = {
    var nextPerm = list
    for (i <- 1 to n) {
      val swapIndex = getNextSwapIndex(nextPerm)
      nextPerm = swap(nextPerm,swapIndex)
      //println(i+":"+nextPerm)
    }
    nextPerm
  }
    
  /**
   * gets the next index where it is possible to 
   * swap and increment the permutation
   */
  def getNextSwapIndex(list:List[Int]):Int = {
    // start at the penultimate list position
    var lastIndex = list.length-2
    (0 to lastIndex).lastIndexWhere(canSwap(list, _))
  }
  
  /**
   * determine whether it is possible to swap at the
   * given index location
   */
  def canSwap(list:List[Int], index:Int) : Boolean = {
    (list(index) < list(index+1))
  }
  
  /**
   * find the next highest number to the listrent lead
   * number from the available remaining numbers
   */
  def getNextHighest(list:List[Int]) :Int = {
    val lead = list.head
    val sorted = list.filter(_ > lead).sorted
    sorted.head
  }
  
  /**
   * order the remaining numbers in the right hand side of the list
   * past the pivot point
   */
  def getRightSide(list:List[Int], index:Int) : List[Int] = {
    val rightSide = list.takeRight(list.length - index)
    val nextHighest = getNextHighest(rightSide)
    List(nextHighest) ::: rightSide.filter(_ != nextHighest).sorted
  }
  
  /**
   * swap out the next available number with the next
   * highest on the right side and return the next permutation
   */
  def swap(list:List[Int], index:Int):List[Int] = {
    list.take(index) ::: getRightSide(list, index)
  }

}