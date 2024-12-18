object Yielder extends App {
    def squares(top: Int): List[Int] = {
        for (x <- (1 to top).toList) yield x * x
    }

    println(squares(10))
}