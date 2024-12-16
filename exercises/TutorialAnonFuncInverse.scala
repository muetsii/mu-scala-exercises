object TutorialAnonFuncInverse extends App {
    def sumInverse(fi: Int => Int, a: Int, sum: Int, result: Int): Int = {
        if (result == sum + a) a
        else sumInverse(fi, a + 1, sum + a, result)
    }

    println(sumInverse(x => x, 1, 0, 55));
}
