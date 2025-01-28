trait Testeable[A, B] {
    def solve(input: A): B
    def inputs(): List[A]
    def expected(): List[B]
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
