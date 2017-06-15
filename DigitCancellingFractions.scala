/**
  * The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may
  * incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and
containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
  */

case class Fraction(numerator: Int, denominator: Int) {
  def isCuriousFraction: Boolean = {
    getCancelledFraction match {
      case Some(f) if f.getRealValue == getRealValue => true
      case _ => false
    }
  }

  def getRealValue: Float = {
    numerator.toFloat / denominator.toFloat
  }

  def mult(f: Fraction): Fraction = {
    Fraction(numerator * f.numerator, denominator * f.denominator)
  }

  def divide(num: Int) = {
    Fraction(numerator /num, denominator / num)
  }

  def getCancelledFraction : Option[Fraction] = {
    val shared = getSharedNums
    if (shared.length > 0) {
      val f = Fraction(removeShared(numerator, shared), removeShared(denominator, shared))
      if (f.isValid)
        return Some(f)
    }
    return None
  }

  def isValid: Boolean = {
    return numerator > 0 && denominator > 0 && numerator != denominator && denominator > numerator
  }

  // remove the shared numbers from the numerator and denominator
  def removeShared(num: Int, shared: List[Int]): Int = {
    val res = num.toString.map(_.asDigit).toList.diff(shared)
    //println("res:" + res)
    if (res.length > 0)
      return res.mkString.toInt
    else
      return 0
  }

  def getSharedNums: List[Int] = {
    val numstr = numerator.toString
    val denstr = denominator.toString
    val allstr = numstr + denstr
    val shared = numstr.collect { case c if denstr.count(_ == c) > 0 => c.asDigit }.toList.distinct
    shared.filter(_ != 0)
  }
}


val fractions = for (numerator <- 10 to 99; denominator <- 10 to 99) yield Fraction(numerator, denominator)

val f = fractions.filter(_.isCuriousFraction).toList

val product = f.foldLeft(Fraction(1,1))((a, b) => a.mult(b)) divide(387296)