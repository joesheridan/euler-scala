
/**
 * If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 
 * 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
 * how many letters would be used?
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
 * The use of "and" when writing out numbers is in compliance with British usage.
 */
object NumberLetterCounts extends App {
  def countNumberLetters(num:Int):String = {
    getThousands(num) + getHundreds(num) + getTensPlusUnits(num)
  }
  
  /**
   * Handle 1000
   */
  def getThousands(num:Int): String = {
    val t = num /1000
    if (t == 1) {
      return "onethousand"
    }
    return ""
  }
  
  def getUnits(units:Int): String = {
    val u = units % 10
    u match {
      case 1 => return "one"
      case 2 => return "two"
      case 3 => return "three"
      case 4 => return "four"
      case 5 => return "five"
      case 6 => return "six"
      case 7 => return "seven"
      case 8 => return "eight"
      case 9 => return "nine"
      case 0 => return ""
    }
  }
    
  def getTeens(num:Int):String = {
    val t = num % 100
    t match {
      case 10 => return "ten"
      case 11 => return "eleven"
      case 12 => return "twelve"
      case 13 => return "thirteen"
      case 14 => return "fourteen"
      case 15 => return "fifteen"
      case 16 => return "sixteen"
      case 17 => return "seventeen"
      case 18 => return "eighteen"
      case 19 => return "nineteen"
    }
  }
  def getTensPlusUnits(num:Int): String = {
    val t = (num / 10) % 10
    t match {
      case 1 => return getTeens(num)
      case 2 => return "twenty" + getUnits(num)
      case 3 => return "thirty" + getUnits(num)
      case 4 => return "fourty" + getUnits(num)
      case 5 => return "fifty" + getUnits(num)
      case 6 => return "sixty" + getUnits(num)
      case 7 => return "seventy" + getUnits(num)
      case 8 => return "eighty" + getUnits(num)
      case 9 => return "ninety" + getUnits(num)
      case 0 => return getUnits(num)
    }
  }
  
  def getHundreds(num:Int):String = {
    val h = (num/100) % 10
    var and = "and"
    if (getTensPlusUnits(num) == "") {
      and = ""
    }
    if (h == 0) return ""
    return getUnits(h) + "hundred" + and
    
  }
  
  var total = 0;
  for (i <- 1 to 1000) {
    total += countNumberLetters(i).length
    println(countNumberLetters(i))
  }
  println("total:"+total)
}