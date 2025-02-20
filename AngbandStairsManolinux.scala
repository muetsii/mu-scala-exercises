import scala.annotation.tailrec

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
object AngbandStairsManolinux extends App with Testeable[List[String], Int] {
    // hack to reuse the test cases
    override def testCases = AngbandStairs.testCases.map(
        tc => TestCase(tc.input, tc.expected)
    )

    val WALL = '#';
    val HERO = '@';
    val EXIT = '>';

    case class Point(y: Int, x: Int)

    /*
    case class Distances(rows: List[List[Option[Int]]], dimensions: Point) extends Matrix {
        def put(distance: Int, point: Point): Distances = {
            if(!inBounds(point)) this
            else {
                import point._
                this.copy( rows = rows.updated(y, rows(y).updated(x, Some(distance))) )
            }
        }
     }
     */
    class FloorMap(grid: List[String]) {
        val dimensionsX = grid(0).length
        val dimensionsY = grid.length

        case class Move(pos: Point, steps: Int)

        class Visited(grid: Array[Array[Boolean]] = Array.fill(dimensionsY)(Array.fill(dimensionsX)(false))) {
            def +(pos: Point) = new Visited(
                grid.updated(pos.y, grid(pos.y).updated(pos.x, true))
            )

            def apply(pos: Point): Boolean = grid(pos.y)(pos.x)
        }

        def outOfBounds(pos: Point): Boolean = pos.x < 0 && pos.y < 0 && pos.x >= dimensionsX && pos.y >= dimensionsY

        def get(pos: Point): Char = {
            // small hack, out of bounds is wall
            if (outOfBounds(pos)) WALL
            else grid(pos.y)(pos.x)
        }

        def locateHero: Point = {
            // FIXME: this is weird, but I dont see another way to do in one line
            grid.map(row => row.indexOf(HERO)).zipWithIndex.filter(
                tuple => tuple._1 != -1
            ).map(tuple => Point(tuple._2, tuple._1)).toList(0)
        }

        def isPassable(pos: Point): Boolean = get(pos) != WALL
        def isExit(pos: Point): Boolean = get(pos) == EXIT

        def getAdjacents(pos: Point): List[Point] = List(
            Point(pos.y + 1, pos.x    ),
            Point(pos.y    , pos.x + 1),
            Point(pos.y - 1, pos.x    ),
            Point(pos.y    , pos.x - 1)
        ).filter(adj => isPassable(adj))

        @tailrec
        final def findExitSteps(
            pending: List[Move] = List(Move(locateHero, 0)),
            visited: Visited = new Visited()
        ): Int = pending match {
            case current :: tail => if (isExit(current.pos)) current.steps
            else findExitSteps(
                if (visited(current.pos)) tail
                else tail ++ getAdjacents(current.pos).map(
                    adj => Move(adj, current.steps + 1)
                ),
                visited + current.pos
            )
            case _ => -1 // never happens if case is correct, but to shut the warning
        }
    }

    override def solve(grid: List[String]): Int = {
        // FIXME: your code here

        val level = new FloorMap(grid)
        level.findExitSteps()
    }

    run()
}
