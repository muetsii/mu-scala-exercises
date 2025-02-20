package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

/*
    Ejemplo de tests en ScalaTest

    Para una lista completa de Matchers, visitar https://www.scalatest.org/user_guide/using_matchers#greaterAndLessThan

*/
class ScalaTestSpec extends AnyFlatSpec with Matchers {
    "substract" should "return a positive number when b - c" in {
        (b - c)  should be > 0
    }

    it should "return a negative number when c - b" in {
        (c - b) should be < 0
    }

    it should "return 0 when b - b" in {
        (b - b) shouldBe 0
    }

    "filter" should "return only elements matching the criteria" in {
        List(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0) should contain theSameElementsAs List(2, 4, 6)
    }

    it should "be defined when Option meets requirement" in {
        Some(2).filter(_ % 2 == 0) shouldBe defined
    }

    "exception" should "be thrown" in {
        class CustomException extends Exception("BOOM!")

        a [CustomException] should be thrownBy {
            throw new CustomException
        }
    }

    private val b: Int = 5
    private val c: Int = 3

}