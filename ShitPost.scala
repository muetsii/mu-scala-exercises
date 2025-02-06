object ShitPost extends App {
    println(List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Do").reduceRight(_ + _))

    println(List(List("x1", "x2", "x3"),List("y1", "y2", "y3"),List("z1", "z2", "z3")).transpose)
    println(List("x1", "x2", "x3") zip List("y1", "y2", "y3") zip List("z1", "z2", "z3"))
}
