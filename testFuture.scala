/**
  * Created by joe on 22/06/2017.
  */

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

def canContinue(): Future[Boolean] = {
  Future.successful(true)
}

def test(): Future[Int] = {

  canContinue().map { b => {
      val a = 8
      if (a > 4) {
        5
      } else
        3
    }
  }

}

test().map(a => {println(a);a})