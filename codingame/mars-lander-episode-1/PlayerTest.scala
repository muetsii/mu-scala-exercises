object PlayerTest extends App {
    object SegmentIntersect extends Testeable[List[Player.Segment], Player.Segment] {
        // [this] <another>
        override def testCases: List[TestCase[List[Player.Segment], Player.Segment]] = List (
            // [<]>
            TestCase(List(
                Player.Segment.create(1, Player.Point(2, 0), Player.Point(6, 0)),
                Player.Segment.create(2, Player.Point(4, 1), Player.Point(10, 3))
            ),  Player.Segment.create(1, Player.Point(2, 3), Player.Point(4, 3))
            ),
            // <[>]
            TestCase(List(
                Player.Segment.create(1, Player.Point(6, 0), Player.Point(12, 0)),
                Player.Segment.create(2, Player.Point(4, 1), Player.Point(10, 3))
            ),  Player.Segment.create(1, Player.Point(10, 3), Player.Point(12, 3))
            ),
            // [<>]
            TestCase(List(
                Player.Segment.create(1, Player.Point(2, 0), Player.Point(6, 0)),
                Player.Segment.create(2, Player.Point(4, 1), Player.Point(5, 3))
            ),  Player.Segment.create(1, Player.Point(2, 3), Player.Point(4, 3))
            ),
            // {]>
            TestCase(List(
                Player.Segment.create(1, Player.Point(0, 0), Player.Point(7, 0)),
                Player.Segment.create(2, Player.Point(0, 1), Player.Point(2, 3))
            ),  Player.Segment.create(1, Player.Point(2, 3), Player.Point(7, 3))
            )

        )

        override def solve(input: List[Player.Segment]): Player.Segment = {
            input(0).intersect(input(1))
        }
    }

    object SegmentIsIntersecting extends Testeable[List[Player.Segment], Boolean] {
            // Play with this points to create the segments
            // p1 q1
            // p2 q2
            val p1 = Player.Point(1, 1)
            val q1 = Player.Point(2, 1)
            val p2 = Player.Point(0, 3)
            val q2 = Player.Point(3, 3)

            val s11 = Player.Segment.create(11, p1, q1)
            val s12 = Player.Segment.create(12, p1, q2)
            val s21 = Player.Segment.create(21, p2, q1)
            val s22 = Player.Segment.create(22, p2, q2)

            override def testCases: List[TestCase[List[Player.Segment], Boolean]] = List (
                TestCase(List(s11, s12), true),
                TestCase(List(s11, s21), true),
                TestCase(List(s11, s22), false),

                TestCase(List(s12, s11), true),
                TestCase(List(s12, s21), true),
                TestCase(List(s12, s22), true),

                TestCase(List(s21, s11), true),
                TestCase(List(s21, s12), true),
                TestCase(List(s21, s22), true),

                TestCase(List(s22, s11), false),
                TestCase(List(s22, s12), true),
                TestCase(List(s22, s21), true)
            )

            override def solve(input: List[Player.Segment]): Boolean = {
                input(0).isIntersecting(input(1))
            }
        }

    object MoonMapIsTheWayFree extends Testeable[List[Player.Point], Boolean] {
        // create a a moon map, similar to game, but simpler
        val moonMap1 = new Player.MoonMap(List(
            Player.Point(1, 2),
            Player.Point(3, 1),
            Player.Point(5, 1),
            Player.Point(6, 5),
            Player.Point(2, 6)
        ))
        // where we want to go
        val target1 = Player.Point(4, 1)

        // spaceship positions to test
        val p1 = Player.Point(0, 1) // false
        val p2 = Player.Point(1, 5) // frue
        val p3 = Player.Point(5, 7) // false
        val p4 = Player.Point(7, 2) // false
        val p5 = Player.Point(4, 4) // true


        override def testCases: List[TestCase[List[Player.Point], Boolean]] = List (
            TestCase(List(p1, target1), false),
            TestCase(List(p2, target1), true),
            TestCase(List(p3, target1), false),
            TestCase(List(p4, target1), false),
            TestCase(List(p5, target1), true)
        )

        override def solve(input: List[Player.Point]): Boolean = {
            // TODO: if I want to test other MoonMap I need to find the way to specify the params
            moonMap1.isTheWayClear(input(0), input(1))
        }
    }

    // we need the moonMap, the ship position and the goal
    // the moonmap will be fixed like in the case before
    // the ship will be input, goal will be the plain segment
    object MoonMap_findNextWayWhenBlocked extends Testeable[Player.Point, Option[Player.Point]] {
        // create a a moon map, similar to game, but simpler
        val moonMap1 = new Player.MoonMap(List(
            Player.Point(1, 2),
            Player.Point(3, 1),
            Player.Point(5, 1),
            Player.Point(6, 5),
            Player.Point(2, 6)
        ), 10, 10)

        // spaceship positions to test
        val p1 = Player.Point(0, 1) // false
        val p2 = Player.Point(1, 5) // frue
        val p3 = Player.Point(5, 7) // false
        val p4 = Player.Point(7, 2) // false
        val p5 = Player.Point(4, 4) // true


        override def testCases: List[TestCase[Player.Point, Option[Player.Point]]] = List (
            TestCase(p3, Option(Player.Point(3, 7)))
        )

        override def solve(ship: Player.Point): Option[Player.Point] = {
            // TODO: if I want to test other MoonMap I need to find the way to specify the params
            moonMap1.findNextWayWhenBlocked(
                ship,
                moonMap1.findNextPlain(4)
            )
        }
    }

    // SegmentIntersect.run()
    // SegmentIsIntersecting.run()
    // MoonMapIsTheWayFree.run()
    MoonMap_findNextWayWhenBlocked.run()
}