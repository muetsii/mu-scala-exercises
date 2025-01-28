object TesteableTester extends App with Testeable[Int, String] {
    override def solve(param: Int) = param match {
        case 1 => "uno"
        case 2 => "dos"
        case 3 => "tres"
        case 4 => "catorce"
    }

    override def inputs(): List[Int] = List(1, 2, 3, 4)
    override def expected(): List[String] = List("uno", "dos", "tres", "cuatro")

    run()
}
