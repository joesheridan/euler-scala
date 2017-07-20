import akka.actor._
import akka.routing._

case class GetTriangleFactors(num:Int)
case class Result(num:Int, triangleNumber: Int)
case class Start()

/**
 * Main app
 */
object HighlyDivisibleTriangle extends App {
  // create the actor system and start it
  override def main(args: Array[String]) {
    val system = akka.actor.ActorSystem("system")
    val router = system.actorOf(Props[MasterActor])
    router ! Start
  } 
}
 
/**
 * Actor which does the factoring number crunching
 */
class FactoringActor extends Actor {
  def getFactors(num: Int) : List[Int] = {
    val range = 2 until num
    range.filter(num % _ == 0).toList
  }
  
  def getTriangleNumber(num: Int) : Int = {
      val range = 1 to num
      range.sum
  }
  
  def receive = {
    case GetTriangleFactors(num) => {
       sender ! Result(getFactors(getTriangleNumber(num)).length, getTriangleNumber(num))
    }
    case _ => println("received unknown message:")
  }
}


/**
 * Master actor creates 8 workers, sends the factoring requests and captures the results
 */
class MasterActor extends Actor {
  val system = akka.actor.ActorSystem("system")
  val router = system.actorOf(Props[FactoringActor].withRouter(RoundRobinPool(8)))
  var highest = 0;
  def receive = {
    case Start => {
      println("masteractor calling gettrianglefactors")
      for (i <- 1 to 100000) {
        router ! GetTriangleFactors(i)
      }
    }
    case Result(num, triangleNumber) => {
      if (num > highest) {
        highest = num
        println("triangle:"+triangleNumber+" highest:" + num)
      }
    }
  }
}

