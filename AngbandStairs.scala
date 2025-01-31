/**
  * Problem: find the way from @ to > and return the number of steps necessary
  *   # are not passable walls.
  *
  * See test cases below, the taks will be clear.
  */


/*

 This is a classic algorithmic problems, with many variations appearing in
 interviews and in algorithmic challenges platforms.

 The way to solve it is to explore the implicit graph in width, keeping track
 of visited nodes to avoid calculating the same cells again and again.

 To explore in width, we use a queue to explore to add the adjacent cells.

 Pseudocode in imperative paradigm:

 ```
 pending = [ [heroStartPos], 0 ]

 for (i = 0; i < pending.length; i++) {
   [pos, steps] = pending[i]

   if (!visited[pos]) {
     if (isExit(pos)) return steps;

     foreach (adjacent in passableAdjacents(pos)) {
       pending.push([adjacent, steps + 1])
     }
   }
 }

 ```
 */
object AngbandStairs extends App with Testeable[List[String], Int] {
    override def testCases: List[TestCase[List[String], Int]] = List(
        TestCase(List(
            "#########",
            "# @   > #",
            "#########"
        ), 4),
        TestCase(List(
            "##############",
            "#       # >  #",
            "#   #   #    #",
            "# @ #   #    #",
            "#   #        #",
            "##############"
        ), 16),
        TestCase(List(
            "##############",
            "#       # >  #",
            "# ### ###    #",
            "# @ #   ###  #",
            "#   # #      #",
            "##############"
        ), 20),
        TestCase(List(
            "#######",
            "#.....#",
            "#.....#",
            "#.@...#",
            "###...#",
            "#.....#",
            "#.>...#",
            "#.....#",
            "#######"
        ), 5)
    )

    override def solve(input: List[String]): Int = 0 // FIXME: your code here

    run()
}
