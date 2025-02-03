import math._
import scala.util._
import scala.io.StdIn._

/**
  * https://www.codingame.com/ide/puzzle/mars-lander-episode-1
 **/
object Player extends App {
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

    val strategy: LandingStrategy = new SpeedLandingStrategy(moonMap, GRAVITY, Range(0, 4), MAX_SPEED)

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
        Console.err.println{s"position $position"}

        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4).
        println(strategy.nextMove(position).toString)
    }

    // Auxiliary classes
    case class Point(x: Int, y: Int)
    case class Segment(s: Point, e: Point) {
        def isInside(x: Double): Boolean = s.x <= x && x <= e.x
        def heightAt(x: Double): Double = ( (x - s.x) / (e.x - s.x)) * s.y + ( (e.x - x)  / (e.x - s.x)) * e.y
    }
    class MoonMap(_points: List[Point]) {
        val points = _points



        private def createSegments(): List[Segment] = {
            (for (i <- 0 until points.length - 1) yield Segment(points(i), points(i + 1))).toList
        }
        val segments = createSegments()
        Console.err.println{s"segments $segments"}


        private def findSegment(x: Int): Segment = {
            // FIXME: should be binary search
            segments.find(s => s.isInside(x)).get
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
        private val threshold = 0.9

        def chooseThrust(p: Position): Int = p.vSpeed match {
            case s if s > maxSpeed / 2 => 0
            case s if s < maxSpeed * threshold => thrusts.end
            case _ => thrusts.end - 1
        }

        override def nextMove(p: Position): Move = Move(chooseThrust(p), 0)
    }
}
