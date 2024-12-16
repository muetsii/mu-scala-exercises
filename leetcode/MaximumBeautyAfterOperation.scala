import scala.annotation.tailrec

/**
  https://leetcode.com/problems/maximum-beauty-of-an-array-after-applying-operation/description/?envType=daily-question&envId=2024-12-11

  ## 2779. Maximum Beauty of an Array After Applying Operation

You are given a 0-indexed array nums and a non-negative integer k.

In one operation, you can do the following:

    Choose an index i that hasn't been chosen before from the range [0, nums.length - 1].
    Replace nums[i] with any integer from the range [nums[i] - k, nums[i] + k].

The beauty of the array is the length of the longest subsequence consisting of equal elements.

Return the maximum possible beauty of the array nums after applying the operation any number of times.

Note that you can apply the operation to each index only once.

  A subsequence of an array is a new array generated from the original array by deleting some elements (possibly none) without changing the order of the remaining elements.
  */


object MaximumBeautyAfterOperation extends App {
  def newBegin
  @tailrec
  def findRange(sorted: List[Int], remaining: List[Int], diameter: Int, begin: Int, maxLength: Int): Int = remaining match {
    case Nil => maxLength
    case head :: tail => {

    }
  }



  def maximumBeauty(nums: Array[Int], radius: Int): Int = {
    printArray(nums.toList)
    radius
  }

  def printArray(nums: List[Int]): Unit = nums match {
    case Nil => {}
    case head :: tail => {
      println(head)
      printArray(tail)
    }
  }

  val nums = Array(4,6,1,2)
  val radius = 2
  println(maximumBeauty(nums, radius))
}


/*
 JS

 /** for each position, the start of the sequence within range */
function dynamicRange(nums, radius) {
    nums.sort((a,b) => a -b);

    let s = 0;
    let maxRange = 0;
    const d = radius * 2;
    for (let i = 1; i < nums.length; i++) {
        while (nums[i] - nums[s] > d) {
            s++;
        }
        if (i - s > maxRange) {
            maxRange = i - s;
        }
    }

    return maxRange + 1;
}

 */