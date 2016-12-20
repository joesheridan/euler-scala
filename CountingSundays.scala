
/**
 * 1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
 * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */
import java.time._

object CountingSundays extends App {
  
  /**
   * Get the day of the week of the date passed in
   */
  def getDateDay(dateStr: String): DayOfWeek = {
    val date = java.time.LocalDate.parse(dateStr)
    date.getDayOfWeek()
  }
  
  /**
   * Increment the month of the datestring passed in
   */
  def getNextDate(dateStr:String): String = {
    val date = java.time.LocalDate.parse(dateStr)
    val nextDate = date.plusMonths(1)
    nextDate.toString()
  }
  
  var curDate = "1901-01-01";
  var sundays = 0;
  while (curDate != "2001-01-01") {
    println(getDateDay(curDate))
    if (getDateDay(curDate) == DayOfWeek.SUNDAY) {
      sundays += 1
    }
    curDate = getNextDate(curDate)
  }
  println("sundays:"+sundays)
}

