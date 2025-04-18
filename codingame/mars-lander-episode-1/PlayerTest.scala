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

    SegmentIntersect.run()
}