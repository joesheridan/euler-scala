/**
 * The following iterative sequence is defined for the set of positive integers:
 *
 * n → n/2 (n is even)
 * n → 3n + 1 (n is odd)
 * 
 * Using the rule above and starting with 13, we generate the following sequence:
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 * 
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
 * Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 * Which starting number, under one million, produces the longest chain?
 */
package collatz

import akka.actor._
import akka.routing._

case class GetCollatzLength(num:Int)
case class Result(num:Int, startNumber:Int)
case class Start()

object LongestCollatzSequence extends App {
  // create the actor system and start it
  override def main(args: Array[String]) {
    val system = akka.actor.ActorSystem("system")
    val router = system.actorOf(Props[MasterActor])
    router ! Start
  } 
}

/**
 * Master actor creates 8 workers, sends the factoring requests and captures the results
 */
class MasterActor extends Actor {
  val system = akka.actor.ActorSystem("system")
  val router = system.actorOf(Props[CollatzActor].withRouter(RoundRobinPool(8)))
  var highest = 0;
  def receive = {
    case Start => {
      println("masteractor dispatching collatz jobs")
      for (i <- 1 to 1000000) {
        router ! GetCollatzLength(i)
      }
    }
    case Result(num, startNumber) => {
      if (num > highest) {
        highest = num
        println("start:"+startNumber+" highest:" + num)
      }
    }
  }
}

/**
 * Actor which does the factoring number crunching
 */
class CollatzActor extends Actor {
  def getCollatzLength(start: Int) : Int = {
    var current = start
    var length = 1
    while(current != 1 && length < 1000) {
      current = getNextCollatzNumber(current)
      length +=1
    }
    return length
  }
  
  def getNextCollatzNumber(num: Int) : Int = {
    if (num == 1) return 1
    if (num % 2 == 0) return (num/2)
    return (num*3 + 1)
  }
  
  def receive = {
    case GetCollatzLength(num) => {
       sender ! Result(getCollatzLength(num), num)
    }
    case _ => println("received unknown message:")
  }
}
