/**
  *  @author Duck.ai
  */

object TwoSegments {
    // Define a case class for a point
    case class Point(x: Double, y: Double)

    // Define a case class for a line segment
    case class LineSegment(p1: Point, p2: Point)

    // Function to calculate the orientation of the triplet (p, q, r)
    // 0 -> p, q and r are collinear
    // 1 -> Clockwise
    // 2 -> Counterclockwise
    def orientation(p: Point, q: Point, r: Point): Int = {
        val val1 = (q.y - p.y) * (r.x - q.x)
        val val2 = (q.x - p.x) * (r.y - q.y)
        if (val1 > val2) 1
        else if (val1 < val2) 2
        else 0
    }

    // Function to check if two line segments intersect
    def doIntersect(p1: Point, q1: Point, p2: Point, q2: Point): Boolean = {
        // Find the four orientations needed for general and special cases
        val o1 = orientation(p1, q1, p2)
        val o2 = orientation(p1, q1, q2)
        val o3 = orientation(p2, q2, p1)
        val o4 = orientation(p2, q2, q1)

        // General case
        if (o1 != o2 && o3 != o4) return true

        // Special cases
        // p1, q1 and p2 are collinear and p2 lies on segment p1q1
        if (o1 == 0 && onSegment(p1, p2, q1)) return true

        // p1, q1 and q2 are collinear and q2 lies on segment p1q1
        if (o2 == 0 && onSegment(p1, q2, q1)) return true

        // p2, q2 and p1 are collinear and p1 lies on segment p2q2
        if (o3 == 0 && onSegment(p2, p1, q2)) return true

        // p2, q2 and q1 are collinear and q1 lies on segment p2q2
        if (o4 == 0 && onSegment(p2, q1, q2)) return true

        false
    }

    // Function to check if point q lies on line segment pr
    def onSegment(p: Point, q: Point, r: Point): Boolean = {
        q.x <= math.max(p.x, r.x) && q.x >= math.min(p.x, r.x) &&
        q.y <= math.max(p.y, r.y) && q.y >= math.min(p.y, r.y)
    }

    def main(args: Array[String]): Unit = {
        val p1 = Point(1, 1)
        val q1 = Point(10, 1)
        val p2 = Point(1, 2)
        val q2 = Point(10, 2)

        val segments = List(
            (p1, q1, p2, q2),
            (p1, q2, p2, q1), // should intersect
            (Point(1, 1), Point(10, 1), Point(1, 2), Point(5, 2)),
            (Point(1, 1), Point(10, 1), Point(1, 2), Point(10, 2))
        )

        segments.foreach { case (p1, q1, p2, q2) =>
            val result = doIntersect(p1, q1, p2, q2)
            println(s"Segments from ($p1) to ($q1) and from ($p2) to ($q2) intersect: $result")
        }
    }

    //main(Array())
}
