import math._
import scala.util._
import scala.io.StdIn._

/**
  * updated for episode 3
  * https://www.codingame.com/ide/puzzle/mars-lander-episode-1
  * https://www.codingame.com/ide/puzzle/mars-lander-episode-2
  * https://www.codingame.com/ide/puzzle/mars-lander-episode-3
 **/
object Player extends App {
    Console.err.println("Marx-Lander from New Soviet Union of Socialist Republics") // Not NASA!
    val GRAVITY = 3.711
    val THRUSTS = Range(0, 4)
    val MAX_SPEED = -40

    val surfaceN = readLine.toInt // the number of points used to draw the surface of Mars.
    val moonMap = new MoonMap((for(i <- 0 until surfaceN) yield {
            // landX: X coordinate of a surface point. (0 to 6999)
            // landY: Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
            val Array(landX, landY) = (readLine split " ").filter(_ != "").map(_.toInt)

            Point(landX, landY)
    }).toList)

    val strategy: LandingStrategy = new CaveLandingStrategy(moonMap, GRAVITY, Range(0, 4), MAX_SPEED)

    // game loop
    while(true) {
        // hSpeed: the horizontal speed (in m/s), can be negative.
        // vSpeed: the vertical speed (in m/s), can be negative.
        // fuel: the quantity of remaining fuel in liters.
        // rotate: the rotation angle in degrees (-90 to 90).
        // power: the thrust power (0 to 4).
        val Array(x, y, hSpeed, vSpeed, fuel, rotate, power) = (readLine split " ").filter(_ != "").map (_.toInt)
        // FIXME: probably can use apply to use directly the Position constructor
        val position = Position(x, y, hSpeed, vSpeed, fuel, rotate, power)
        
        // 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4).
        println(strategy.nextMove(position).toString)
    }

    // Auxiliary classes
    case class Point(x: Int, y: Int) {
        def min(another: Point) = if (this.x <= another.x) this else another
        def max(another: Point) = if (this.x <= another.x) another else this
        def distanceTo(another: Point): Double = math.sqrt(
            math.pow(this.x - another.x, 2) + math.pow(this.y - another.y, 2)
        )
    }

    case class Segment(i: Int, s: Point, e: Point, plain: Boolean) {
        def isInside(x: Double): Boolean = s.x <= x && x <= e.x
        def heightAt(x: Double): Double = ( (x - s.x) / (e.x - s.x)) * s.y + ( (e.x - x)  / (e.x - s.x)) * e.y
        def maxHeight(): Int = if (s.y > e.y) s.y else e.y
        def minHeight(): Int = if (s.y > e.y) e.y else s.y
        def pointIsInside(point: Point): Boolean = point.x >= s.x && point.x <= e.x
        def pointIsInsideUnderHeight(point: Point, height: Int): Boolean = {
            point.y >= height && pointIsInside(point)
        }

        def isSegmentColliding(anotherSegment: Segment): Boolean = {
            (anotherSegment.s.x < this.s.x && anotherSegment.e.x >= this.s.x) ||
            (anotherSegment.s.x >= this.s.x && anotherSegment.s.x <= this.e.x)
        }

        def isSegmentCollidingUnderHeight(anotherSegment: Segment, height: Int) = {
            anotherSegment.minHeight <= height &&
            isSegmentColliding(anotherSegment)
        }

        // assumes the segments intersect, and there would be an open space
        // and anotherSegment is over this segment
        // FIXME? for the moment, also that there is only one aperture
        def intersect(anotherSegment: Segment): Segment = {
            if (this.i == anotherSegment.i) return this

            val sx = if (anotherSegment.s.x == this.s.x) anotherSegment.e.x
            else if (anotherSegment.s.x < this.s.x) anotherSegment.e.x
            else this.s.x
            val ex = if (anotherSegment.e.x == this.e.x) anotherSegment.e.x
            else if (anotherSegment.s.x <= this.s.x) this.e.x
            else anotherSegment.s.x
            val h = anotherSegment.maxHeight

            Segment(this.i, Point(sx, h), Point(ex, h), true)
        }

        /** from a list of another segments, create an imaginary segment which is the area not covered with highest height */
        def aperture(p: Position, allSegments: List[Segment], intersection: Segment = this): Segment = allSegments match {
            case Nil => intersection
            case head :: tail => {
                if (intersection.isSegmentCollidingUnderHeight(head, p.y)) aperture(
                    p,
                    tail,
                    intersection.intersect(head)
                ) else aperture(
                    p,
                    tail,
                    intersection
                )
            }
        }

        def rise(newHeight: Int): Segment = Segment(i, Point(s.x, newHeight), Point(e.x, newHeight), true)

        /** is point p inside segment (s - e)? */
        def isPointOnSegment(p: Point): Boolean = {
            p.x <= math.max(s.x, e.x) && p.x >= math.min(s.x, e.x) &&
            p.y <= math.max(s.y, e.y) && p.y >= math.min(s.y, e.y)
        }

        def isIntersecting(another: Segment): Boolean = {
            // Find the four orientations needed for general and special cases
            val o1 = Point.orientation(this.s, this.e, another.s)
            val o2 = Point.orientation(this.s, this.e, another.e)
            val o3 = Point.orientation(another.s, another.e, this.s)
            val o4 = Point.orientation(another.s, another.e, this.e)

            // general case
            (o1 != o2 && o3 != o4) ||
            // segment containing a point of the other
            (o1 == Point.ORIENTATION_COLLINEAR && this.isPointOnSegment(another.s)) ||
            (o2 == Point.ORIENTATION_COLLINEAR && this.isPointOnSegment(another.e)) ||
            (o3 == Point.ORIENTATION_COLLINEAR && another.isPointOnSegment(this.s)) ||
            (o4 == Point.ORIENTATION_COLLINEAR && another.isPointOnSegment(this.e))
        }
    }

    object Point {
        val ORIENTATION_COLLINEAR = 0
        val ORIENTATION_CLOCKWISE = 1
        val ORIENTATION_COUNTERCLOCKWISE = 2
        /**
          *  Find the orientation.
          *  Inspired in Duck.ai, inspired in https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
          * Used for check intersection
          */
        def orientation(p: Point, q: Point, r: Point): Int = {
            val val1 = (q.y - p.y) * (r.x - q.x)
            val val2 = (q.x - p.x) * (r.y - q.y)
            if (val1 > val2) ORIENTATION_CLOCKWISE
            else if (val1 < val2) ORIENTATION_COUNTERCLOCKWISE
            else ORIENTATION_COLLINEAR
        }
    }

    object Segment {
        def create (i: Int, p1: Point, p2: Point): Segment = {
            Segment(i, p1.min(p2), p1.max(p2), p1.y == p2.y)
        }
    }

    class MoonMap(_points: List[Point], _max_x: Int = 7000, _max_y: Int = 3000) {
        val points = _points
        val max_x = _max_x
        val max_y = _max_y

        private def createSegments(): List[Segment] = {
            (for (i <- 0 until points.length - 1) yield Segment.create(i, points(i), points(i + 1))).toList
        }
        val segments = createSegments()
        Console.err.println{s"segments $segments"}


        private def findSegment(x: Int): Segment = {
            // FIXME: should be binary search
            segments.find(s => s.isInside(x)).get
        }

       def findNextPlain(x: Int): Segment = {
            val current = findSegment(x)

            if (current.plain) current
            else findNextPlainNearTo(x, current.i, 1)
        }

        private def findNextPlainNearTo(x: Int, i: Int, offset: Int): Segment = {
            val ol = if (i - offset >= 0 && segments(i - offset).plain) Some(segments(i - offset)) else None
            val or = if (i + offset < segments.length && segments(i + offset).plain) Some(segments(i + offset)) else None

            // FIXME: I think can be simpler with better use of Option
            // FIXME: segment at i+2 could be actually further than i-3, but let's fix only if it's a problem
            (ol, or) match {
                case (Some(l), Some(r)) => if (x - l.e.x < r.s.x - x) l else r
                case (Some(l), None) => l 
                case (None, Some(r)) => r
                case _ => findNextPlainNearTo(x, i, offset + 1)
            }
        }

        def heightAt(x: Int): Double = findSegment(x).heightAt(x)

        // ++ Methods for "djikstra"
        def isOutOfBounds(p: Point): Boolean = {
            (p.x < 0 || p.x >= max_x || p.y < 0 || p.y >= max_y)
        }

        def isTheWayClear(p1: Point, p2: Point): Boolean = {
            val way = Segment.create(-1, p1, p2)

            // isPointOnSegment because we admit that p2 is on the goal
            segments.forall(s =>  !isSegmentIntersecting(way, s) || intersectException(p1, p2, s))
        }
        def isTheWayClear(ship: Point, goal: Segment): Boolean = {
            // we'll check the way to each extreme
            // not perfect but hopefully will work
            // we dont choose exact extremes bc they are shared
            // with other segments
            isTheWayClear(ship, Point(goal.s.x + 1, goal.s.y)) || isTheWayClear(ship, Point(goal.e.x - 1, goal.e.y))
        }

        // the one to avoid that it collides with the goal
        def intersectException(p1: Point, p2: Point, segment: Segment) = {
            (segment.plain && (segment.isPointOnSegment(p2) || (segment.isPointOnSegment(p1))))
        }

        def isSegmentIntersecting(way: Segment, surface: Segment): Boolean = surface.isIntersecting(way)

        val allDirections = List(
            Point(-1,  1),
            Point(-1, -1),
            Point(-1,  0),
            Point( 0, -1),
            Point( 0,  1),
            Point( 1, -1),
            Point( 1,  0),
            Point( 1,  1)
        )
        /** Gets "adjacents", adjacents are points in the "eight directions" filtering those
          *  intersecting with the surface
          */
        def getAdjacents(
            ship: Point,
            increment: Int,
            visited: Set[Point]
        ): List[Point] = {
            allDirections.map(
                // direction is reversed to easily pass the opposite direction
                d => Point(ship.x + d.x * increment, ship.y + d.y * increment)
            ).filter(
                destination => !isOutOfBounds(destination) && !visited.contains(destination) && isTheWayClear(ship, destination)
            )
        }

        val DIST_DIVISION = 3
        case class Step(pos: Point, firstJump: Option[Point] = None)
        /** Find a point in the middle of a way to the goal, djikstra style */
        def findNextWayWhenBlocked(
            ship: Point,
            goal: Segment
        ): Option[Point] = {
            // we calculate the increment arbirarily to the start, shouldn't matter too much
            val increment = (max_x.min(max_y) / DIST_DIVISION).round.toInt
            findNextWayWhenBlocked(List(Step(ship, None)), goal, increment)
        }

        /** Find a point in the middle of a way to the goal, djikstra style */
        def findNextWayWhenBlocked(
            pending: List[Step],
            goal: Segment,
            increment: Int,
            visited: Set[Point] = Set[Point]()
        ): Option[Point] = pending match {
            case Nil => None
            case head :: tail => {
                val pos = head.pos
                if (!visited.contains(pos)) {
                    if (
                        isTheWayClear(pos, goal)
                    )  head.firstJump
                    else {
                        val nextVisited = visited + pos
                        val newSteps = getAdjacents(pos, increment, visited).map(
                            adj => Step(adj, Option(head.firstJump.getOrElse(adj)))
                        )
                        val nextPending = tail ++ newSteps

                        findNextWayWhenBlocked(
                            nextPending,
                            goal,
                            increment,
                            nextVisited
                        )
                    }
                } else findNextWayWhenBlocked(
                    tail,
                    goal,
                    increment,
                    visited
                )
            }
        }
    }

    case class Position(x: Int, y: Int, hSpeed: Int, vSpeed: Int, fuel: Int, rotate: Int, power: Int) {
        val toPoint: Point = Point(x, y)
    }
    case class Move(thrust: Int, angle: Double) {
        // for the moment I'm not sure if I will use degrees or radians
        // for the moment it's in degrees, just make it Int
        def toIntDegreeAngle(): Int = angle.toInt;
        override def toString() = s"${toIntDegreeAngle()} $thrust"
    }
    abstract class LandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) {
        val map = _map
        val gravity = _gravity
        val thrusts = _thrusts
        val maxSpeed = _maxSpeed

        def verticalAcc(angle: Double, thrust: Int): Double = {
            // TODO: consider angle
            thrust - gravity;
        }

        /**
          * If we are going too far and we dont have time to reduce the speed in the remaining distance, we are dead
          *
          * @param p
          * @return
          */
        def isSolvable(angle: Double, height: Double, speed: Double): Boolean = {
            (brakingDistance(angle, speed) <= height)
        }

        /**
         * Distance we traverse if we go full thrust
         */
        def brakingDistance(angle: Double, speed: Double): Double = {
            // d = s * t + a * t^2

            // s = a * t <== only for 0
            // t = s / a

            // s1 = s + a * t
            // t = a / (s1 - s)
            // s1, the speed I want is -40 m/s
            val acceleration = verticalAcc(angle, thrusts.end)

            val time = ((-40 - speed) / acceleration).abs
            -(speed * time + acceleration * time * time)
        }

        /**
          * Return how much space we will move, and how fast we will go
          *
          * @param angle
          * @param speed
          */
        def whatIfWeWentAt(angle: Double, speed: Double, power: Int): (Double, Double) = {
            val time = 1
            val newSpeed = speed + verticalAcc(angle, power) * time
            val distance = speed * time + verticalAcc(angle, power) * time * time

            (distance, newSpeed)
        }

        def calculateTimeToFall(speed: Int, height: Int): Double = {
            // d = s * t + 0.5 * a * t^2
            (-speed + scala.math.sqrt(speed*speed + 2 * gravity * height)) / gravity
        }

        def calculateCurrentLandingPoint(p: Position, h: Int): Double = {
            p.x + p.hSpeed * calculateTimeToFall(p.vSpeed, h)
        }

        def nextMove(p: Position): Move
    }

    /**
      * This strategy is very simple, it considers only vertical movement, and does not
      * try to optimize fuel.
      *
      * It returns max if we are going down, or just below gravity if going up
      */ 
    class InfiniteFuelVerticalLandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) extends LandingStrategy(_map, _gravity, _thrusts, _maxSpeed) {
        val descensePower = thrusts.find(t => t >= gravity).getOrElse(thrusts.end) - 1;

        override def nextMove(p: Position): Move = Move(if (p.vSpeed < 0) thrusts.end else descensePower, 0)
    }

    /**
      * Save fuel, but only consider vertical height
      *
      *  It tries to guess when the game will become unable to solve, and then brake
      *  But does not work for the moment.
      */
    class VerticalLandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) extends LandingStrategy(_map, _gravity, _thrusts, _maxSpeed) {
        val slowDescendentPower = thrusts.find(t => t >= gravity).getOrElse(thrusts.end) - 1;

        def chooseThrust(p: Position): Int = {
            val (distance, potentialSpeed) = whatIfWeWentAt(0, p.vSpeed, 0)
            val potentialHeight = p.y - map.heightAt(p.x) + distance

            if (isSolvable(0, potentialHeight, potentialSpeed)) 0
            else if (p.vSpeed < 0) thrusts.end else slowDescendentPower
        }

        override def nextMove(p: Position): Move = Move(chooseThrust(p), 0)
    }

    /**
      * Just try to keep under 40 m/s
      */
    class SpeedLandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) extends LandingStrategy(_map, _gravity, _thrusts, _maxSpeed) {
        val threshold = 0.9

        def chooseThrust(p: Position): Int = p.vSpeed match {
            case s if s > maxSpeed / 2 => 0
            case s if s < maxSpeed * threshold => thrusts.end
            case _ => thrusts.end - 1
        }

        override def nextMove(p: Position): Move = Move(chooseThrust(p), 0)
    }
    /**
      * Just try to keep under 40 m/s
      */
    class Speed2DLandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) extends SpeedLandingStrategy(_map, _gravity, _thrusts, _maxSpeed) {
        val rotation = 45

        def setGoal(p: Position): Segment = map.findNextPlain(p.x)

        def isTooFast(speed: Int): Int = {
                if (speed > -(maxSpeed / 2) * threshold ) 1
                else if (speed < (maxSpeed / 2) * threshold) -1
                else 0
        }

        def chooseAngle(p: Position): Int = {
            val goal = setGoal(p)
            val nextX = calculateCurrentLandingPoint(p, goal.s.y)

            if (goal.isInside(nextX)) isTooFast(p.hSpeed) * rotation
            else if (nextX > goal.e.x && !(isTooFast(p.hSpeed) == -1)) rotation
            else if (nextX < goal.s.x && !(isTooFast(p.hSpeed) == +1)) -rotation
            else 0
        }

        override def chooseThrust(p: Position): Int = {
            val goal = setGoal(p)
            val nextX = calculateCurrentLandingPoint(p, goal.s.y)            

            if (goal.isInside(nextX) && isTooFast(p.hSpeed) == 0) super.chooseThrust(p)
            else thrusts.end
        }

        override def nextMove(p: Position): Move = Move(chooseThrust(p), chooseAngle(p))
    }

    /** Class to overcome cave obstacles. It works under the assumption that some of the goal
      will be exposed to the sky, like in the test cases

      The asumption was wrong. My sight is terrible, but actually the goal is totally covered.
      We need another Strategy.

      Based in Speed2DLandingStrategy
      */
    class OpenCaveLandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) extends Speed2DLandingStrategy(_map, _gravity, _thrusts, _maxSpeed) {
        val points = _map.points.sortWith(_.x < _.x)

        def getObstacleStartEnd(x: Int, goal: Segment) = {
            if (x < goal.s.x) (x, goal.e.x)
            else if (x > goal.e.x) (goal.s.x, x)
            else (goal.s.x, goal.e.x)
        }

        def riseGoalIfObstacles(goal: Segment, sx: Int, ex: Int, points: List[Point]): Segment = points match {
            case Nil => goal
            case head :: tail => {
                if (head.x > ex) goal
                else if (head.x < sx) riseGoalIfObstacles(goal, sx, ex, tail)
                else if (head.y > goal.s.y) riseGoalIfObstacles(goal.rise(head.y), sx, ex, tail)
                else riseGoalIfObstacles(goal, sx, ex, tail)
            }
        }

        override def setGoal(p: Position): Segment = {
            val aperture = super.setGoal(p).aperture(p, map.segments)
            if (aperture.isInside(p.x)) aperture
            else {
                val sx = aperture.s.x.min(p.x)
                val ex = aperture.e.x.max(p.x)
                riseGoalIfObstacles(aperture, sx, ex, points)
            }

        }

        override def chooseAngle(p: Position): Int = {
            val goal = setGoal(p)

            if (goal.s.y >= p.y) 0
            else super.chooseAngle(p)
        }

        override def chooseThrust(p: Position): Int = {
            val goal = setGoal(p)

            if (goal.s.y >= p.y) thrusts.end
            else super.chooseThrust(p)
        }
    }

    /**
      *  Strategy using djikstra if the cave is closed
      */
    class CaveLandingStrategy(_map: MoonMap, _gravity: Double, _thrusts: Range, _maxSpeed: Int) extends Speed2DLandingStrategy(_map, _gravity, _thrusts, _maxSpeed) {
        override def setGoal(p: Position): Segment = {
            val goal = super.setGoal(p)

            if (moonMap.isTheWayClear(p.toPoint, goal.s)) {
                if (moonMap.isTheWayClear(p.toPoint, goal.e)) goal
                else Segment.create(-3, Point(goal.s.x, goal.s.y), Point(goal.s.x + 1, goal.s.y))
            } else if (moonMap.isTheWayClear(p.toPoint, goal.e))
                Segment.create(-3, Point(goal.s.x, goal.s.y), Point(goal.s.x + 1, goal.s.y))
            else {
                val nextDest = moonMap.findNextWayWhenBlocked(p.toPoint, goal)

                nextDest match {
                    case None => {
                        Console.err.println(s"DANGER: we didnt find a way to goal $goal")
                        goal
                    }
                // turn destination point into a tiny goal
                    case Some(d) => pointToGoal(p, d)
                }
            }
        }

        def pointToGoal(ship: Position, p: Point): Segment = {
            // Segment.create(-1, p, Point(d.p + 1, p.y))
            val dx = ship.x - p.x
            val dy = ship.y - p.y

            val x = if (dx >= 0) 0 else moonMap.max_x - 1
            val y = if (dy >= 0) 0 else moonMap.max_y - 1
            Segment.create(
                -1,
                Point(x, y),
                Point(x + 1, y)
            )
        }

        override def chooseAngle(p: Position) = {
            if (setGoal(p).s.y > p.y && p.vSpeed <= 0) 0
            else super.chooseAngle(p)
        }

        override def chooseThrust(p: Position): Int = {
            val goal = setGoal(p)

            if (goal.s.y >= p.y) thrusts.end
            else if (p.vSpeed >= 20) 0
            else if (p.y >= 2500 && p.vSpeed > 0) 0
            else super.chooseThrust(p)
        }
    }
}
