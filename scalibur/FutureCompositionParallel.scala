import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random


// This should work, but requires scala 3


object FutureCompositionParallel extends App {

    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    // Simulates a costy operation, sleeping a random time
    // Takes seconds
    def costyOperation(a: Int): Future[Int] = Future {
        val cost = a
        Thread sleep cost * 1000
        a*10
    }
    
    def sumCostyOperations(a: Int, b: Int, c: Int): Future[Int] = {
        Future.reduceLeft(
            List(a, b, c).map(x => costyOperation(x))
        )((x, y) => x + y)
    }

    // Does not have time to wait for 1 + 2 + 3 = 6 seconds!
    val result = Await.result(sumCostyOperations(1, 2, 3), 4 seconds)
    println(s"$result should be 60")

}
