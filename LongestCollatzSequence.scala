import akka.actor._
import akka.routing._
package com.me.collatz {

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
}