trait Testeable[A, B] {
    case class TestCase[A1, B1] (input: A1, expected: B1)

    def solve(input: A): B

    def testCases(): List[TestCase[A, B]]
    def inputs(): List[A] = testCases().map(t => t.input)
    def expected(): List[B] = testCases().map(t => t.expected)
    def compare(expected: B, actual: B): Boolean = expected equals actual

    def run(): Unit = {
        val cases  = testCases()

        cases.foreach(tc => {
            val out = solve(tc.input)

            if (compare(tc.expected, out)) {
                println(s"OK    ${tc.input} -> ${tc.expected}")
            } else {
                println(s"ERROR ${tc.input} -> ${tc.expected} / $out")
            }

        })
    }
}
