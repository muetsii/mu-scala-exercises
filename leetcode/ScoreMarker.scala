import scala.annotation.tailrec

object ScoreMarker {
    @tailrec
    def findAndMark(sorted: List[(Int,Int)], marked: Set[Int], score: Int): Long = sorted match  {
        case Nil => return score
        case head :: tail => {
            val hit = !marked.contains(head._2)
            val newScore = if (hit) (score + head._1) else score
            val newMarked = if (hit) marked + head._2 - 1 + head._2 + head._2 + 1 else marked

            findAndMark(tail, newMarked, newScore)
        }
    }
    def findScore(nums: Array[Int]): Long = findAndMark(
        nums.toList.zipWithIndex.sortWith(_._2 < _._2), Set[Int](), 0
    )
}
