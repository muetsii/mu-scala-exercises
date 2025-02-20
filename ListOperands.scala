object ListOperands extends App {
    println(List("hello") +: "world")
    println(List("hello") :+ "world")
    println(List("hello", "world") ++ List("how", "are", "you"))
}
