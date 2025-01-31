object PartiallyApplied extends App {
    def multiply(a: Int, b: Int): Int = a * b

    val double = multiply(2, _: Int)

    println(double(33)) // 66
}