import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random

object FutureComposition extends App {

    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    // Simulates a costy operation, sleeping a random time
    def costyOperation(a: Int): Future[Int] = Future {
        val cost = Random.nextInt(10)
        Thread sleep cost * 1000
        a*10
    }
    
    def sumCostyOperations(a: Int, b: Int, c: Int): Future[Int] = {
        // this will wait for each operation sequentially!
        // (it was the expected solution, actually)
        for {
            ra <- costyOperation(a)
            rb <- costyOperation(b)
            rc <- costyOperation(c)
        } yield ra + rb + rc
    }
    
    
    val result = Await.result(sumCostyOperations(1, 2, 3), 1 minute)
    println(s"$result should be 60")

}
