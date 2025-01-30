trait Testeable[A, B] {
    case class TestCase[A1, B1] (input: A1, expected: B1)

    def solve(input: A): B

    def testCases(): List[TestCase[A, B]]
    def inputs(): List[A] = testCases().map(t => t.input)
    def expected(): List[B] = testCases().map(t => t.expected)
    def compare(expected: B, actual: B): Boolean = expected equals actual

    def run(): Unit = {
        val in = inputs()
        val ex = expected()

        for (i <- 0 until in.length) {
            val out = solve(in(i))

            if (compare(ex(i), out)) {
                println(s"OK    ($i) ${in(i)} -> ${ex(i)}")
            } else {
                println(s"ERROR ($i) ${in(i)} -> ${ex(i)} / $out")
            }
        }
    }
}
