/**
  * An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  */

def genFraction(d:Int): String = {
  var max = d
  if (d > 10000)
    max = d / 2
  if (d == 100000)
    max = 30000
  if (d == 1000000)
    max = 250000
  (1 to max).foldLeft("")(_ + _)
}

genFraction(10)

def getDigit(d: Int): BigInt = {
  val num = genFraction(d)
  BigInt(num(d - 1).asDigit)
}

val a = getDigit(1)
val b = getDigit(10)
val c = getDigit(100)
val d = getDigit(1000)
val e = getDigit(10000)
val f = getDigit(100000)
val g = getDigit(1000000)
val i:BigInt = a * b * c * d * e * f * g

