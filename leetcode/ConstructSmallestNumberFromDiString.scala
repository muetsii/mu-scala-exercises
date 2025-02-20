/**

  https://leetcode.com/problems/construct-smallest-number-from-di-string/
  */

class ConstructSmallestNumberFromDiString {
    class SequenceBuilder(pattern: String) {
        def isInc(i: Int): Boolean = pattern.charAt(i) == 'I'
        def calculateIncs(tail: List[Int] = List(0), j: Int = pattern.length - 1): List[Int] = {
            if (j < 0) tail
            else calculateIncs(
                (if (isInc(j)) 0 else tail(0) + 1) :: tail,
                j - 1
            )
        }

        def firstAvailable(used: Array[Boolean]): Int = used.indexWhere(u => !u) + 1

        // setup initial used
        val unused: Array[Boolean] = Array.fill(10)(false)
        def use(used: Array[Boolean], n: Int): Array[Boolean] = used.updated(n - 1, true)

        def allocate(nums: List[Int] = Nil, used: Array[Boolean] = unused, incs: List[Int] = calculateIncs()): List[Int] = incs match {
            case Nil => nums
            case head :: tail => {
                val next = firstAvailable(used) + head
                allocate(nums :+ next, use(used, next), tail)
            }
        }
    }

    def smallestNumber(pattern: String): String = {
        new SequenceBuilder(pattern).allocate().mkString("")
    }

}
