/**
  https://leetcode.com/problems/maximum-absolute-sum-of-any-subarray
  */

object MaximumAbsoluteSumOfSubarrays extends App with Testeable[Array[Int], Int] {
    trait Counter {
        def :+(n: Int): Counter
        def apply(): Int
    }

    class PositiveCounter(sum: Int = 0, max: Int = 0) extends Counter {
        def :+(n: Int): Counter = {
            val newSum = sum + n
            if (newSum < 0) new PositiveCounter(0, max)
            else new PositiveCounter(
                newSum,
                max.max(newSum)
            )
        }

        def apply(): Int = max
    }

    class AbsCounter(
        positive: Counter = new PositiveCounter(),
        negative: Counter = new PositiveCounter()
    ) extends Counter {
        def :+(n: Int): Counter = new AbsCounter(
            positive :+  n,
            negative :+ -n
        )

        def apply(): Int = positive().max(negative())
    }

    def masRecursive(nums: List[Int], counter: Counter = new AbsCounter()): Int = nums match {
        case Nil => counter()
        case n :: tail => masRecursive(tail, counter :+ n)
    }

    def maxAbsoluteSum(nums: Array[Int]): Int = masRecursive(nums.toList)

    override val testCases: List[TestCase[Array[Int], Int]] = List(
        TestCase(
            Array(1,-3,2,3,-4),
            5
        ),
        TestCase(
            Array(2,-5,1,-4,3,-2),
            8
        )
    )

    override def solve(input: Array[Int]): Int = maxAbsoluteSum(input)

    run()
}
