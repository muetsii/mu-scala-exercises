object ShitPost extends App {
    println(List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Do").reduceRight(_ + _))

    println(List(List("x1", "x2", "x3"),List("y1", "y2", "y3"),List("z1", "z2", "z3")).transpose)
    println(List("x1", "x2", "x3") zip List("y1", "y2", "y3") zip List("z1", "z2", "z3"))

    def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
        "%d %ss can give you %s".format(x, y, z.mkString(", "))
    }

    println(repeatedParameterMethod(
        3,
        "egg",
        "a delicious sandwich",
        "protein",
        "high cholesterol"
    ))

    println(repeatedParameterMethod(
        3,
        "egg",
        List("a delicious sandwich", "protein", "high cholesterol")
    ))

    println(Array.fill(10)(false).toList)
    println("a char"(2).getClass)
}
