/**
  * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  */

def getFactorial(num: Int): Int = {
  if (num == 0)
    return 0
  if (num == 1)
    return 1
  num * getFactorial(num -1)
}

def isDigitFactorial(num: Int): Boolean = {
  val sum = num.toString.map(_.asDigit).map(getFactorial(_)).toList.sum
  num == sum
}

isDigitFactorial(145)
isDigitFactorial(146)

(3 to 14600).filter(isDigitFactorial(_)).toList