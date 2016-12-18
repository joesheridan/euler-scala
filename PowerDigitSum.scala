
/**
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 * What is the sum of the digits of the number 2^1000?
 */
object PowerDigitSum extends App {
   val res = BigInt(2).pow(1000)
   var total = 0
   res.toString.foreach(c => { total += c.asDigit }) 
   println("res:"+res.toString)
   println("total of digits:" + total)
}