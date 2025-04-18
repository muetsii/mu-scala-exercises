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

    val strategy: LandingStrategy = new OpenCaveLandingStrategy(moonMap, GRAVITY, Range(0, 4), MAX_SPEED)

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

    }

    object Segment {
        def create (i: Int, p1: Point, p2: Point): Segment = {
            Segment(i, p1.min(p2), p1.max(p2), p1.y == p2.y)
        }
    }

    class MoonMap(_points: List[Point]) {
        val points = _points



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
    }

    case class Position(x: Int, y: Int, hSpeed: Int, vSpeed: Int, fuel: Int, rotate: Int, power: Int)
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

}
