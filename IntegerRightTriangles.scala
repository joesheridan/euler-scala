/**
  * If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
  * there are exactly three solutions for p = 120.
  *
  * {20,48,52}, {24,45,51}, {30,40,50}
  *
  * For which value of p ≤ 1000, is the number of solutions maximised?
  */

case class Triangle(side1: Int, side2: Int, hypotenuse: Int)

def getHypotenuse(i: Int, j: Int): Double = {
  val ysquared: Double = (i*i) + (j*j)
  Math.sqrt(ysquared)
}

def getSolutions(p: Int): Seq[Triangle] = {
  for {
    i <- 1 to p
    j <- 1 to p
    if (getHypotenuse(i, j) + i + j == p) && i <= j
  } yield Triangle(i, j, getHypotenuse(i, j).toInt)
}

getHypotenuse(3,4)
getSolutions(120)

Stream.iterate(1)(_ + 1).take(1000).collect {
  case i:Int if getSolutions(i).length > 4 => (i, getSolutions(i).length)
}.toList.maxBy(_._2)
