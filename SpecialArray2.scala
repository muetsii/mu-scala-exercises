// https://leetcode.com/problems/special-array-ii/?envType=daily-question&envId=2024-12-09
import scala.annotation.tailrec

/**
 An array is considered special if every pair of its adjacent elements contains two numbers with different parity.

You are given an array of integer nums and a 2D integer matrix queries, where for queries[i] = [fromi, toi] your task is to check that
subarray
nums[fromi..toi] is special or not.

Return an array of booleans answer such that answer[i] is true if nums[fromi..toi] is special.

 

Example 1:

Input: nums = [3,4,1,2,6], queries = [[0,4]]

Output: [false]

Explanation:

The subarray is [3,4,1,2,6]. 2 and 6 are both even.

Example 2:

Input: nums = [4,3,1,6], queries = [[0,2],[2,3]]

Output: [false,true]

Explanation:

    The subarray is [4,3,1]. 3 and 1 are both odd. So the answer to this query is false.
    The subarray is [1,6]. There is only one pair: (1,6) and it contains numbers with different parity. So the answer to this query is true.

 

Constraints:

    1 <= nums.length <= 105
    1 <= nums[i] <= 105
    1 <= queries.length <= 105
    queries[i].length == 2
    0 <= queries[i][0] <= queries[i][1] <= nums.length - 1

 */


object SpecialArray2 {
  val inputs = List(
    Map(
      "nums" -> Array[Int](3,4,1,2,6),
      "queries" -> Array[Array[Int]](Array[Int](0,4))
    ),
    Map(
      "nums" -> Array[Int](4,3,1,6),
      "queries" -> Array[Array[Int]](Array[Int](0,2),Array[Int](2,3))
    )
  )

  def getValue(nums: Array[Int], i: Int, last: Int): Int = {
    if (nums(i) % 2 != nums(i - 1) % 2) last
    else i
  }

  def subspecials(nums: Array[Int]): List[Int] = {
    List[Int](0) ++ subspecials(nums, 1, 0, List())
  }

  @tailrec
  def subspecials(nums: Array[Int], i: Int, last: Int, specials: List[Int]): List[Int] = {
    if (i == nums.length) specials
    else {
      val value = getValue(nums, i, last)
      val nextLast = if (value == last) last else value

      subspecials(nums, i + 1, nextLast, specials ++ List(value))
    }
  }

  def isSpecial(specials: List[Int], start: Int, end: Int): Boolean = {
    start >= specials(end)
  }

  def isArraySpecial(nums: Array[Int], queries: Array[Array[Int]]): Array[Boolean] = {
    val specials = subspecials(nums)
    println("BORRAR: specials")
    println(specials)

    queries.map(q => isSpecial(specials, q(0), q(1)))
  }

  def main(args: Array[String]): Unit = {
    println(
      inputs.map(data => {
        val nums = data("nums").asInstanceOf[Array[Int]]
        val queries = data("queries").asInstanceOf[Array[Array[Int]]]

        isArraySpecial(nums, queries).toList
      })
    )
  }
}

/*
 JS implementation


 

function getSubSpecialArrays(nums) {
    const specials = new Array(nums.length);
    specials[0] = 0;
    let lastOddity = nums[0] % 2;
    let last = 0;

    for (let i = 1; i < nums.length; i++) {
        const oddity = nums(i) %2;

        if (oddity != lastOddity) {
            specials[i] = last;
            lastOddity = oddity;
        } else {
            specials[i] = last = i;
        }
    }

    return specials;
}

function isSpecial(specials, [start, end]) {
    return start >= specials[end];
}

function checkSpecials(nums, queries) {
    const specials = getSubSpecialArrays(nums);
    //console.debug(specials);

    return queries.map(q => isSpecial(specials, q));
}

/**
 * @param {number[]} nums
 * @param {number[][]} queries
 * @return {boolean[]}
 */
var isArraySpecial = checkSpecials;


*/
