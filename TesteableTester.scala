object TesteableTester extends App with Testeable[Int, String] {
    override def solve(param: Int) = param match {
        case 1 => "uno"
        case 2 => "dos"
        case 3 => "tres"
        case 4 => "catorce"
    }

    override def testCases(): List[TestCase[Int, String]] = List(
        TestCase(1, "uno"),
        TestCase(2, "dos"),
        TestCase(3, "tres"),
        TestCase(4, "cuatro")
    )

    run()
}
