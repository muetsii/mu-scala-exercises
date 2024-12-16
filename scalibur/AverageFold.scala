
object AverageFold extends App {
    // Calcula la media sin usar recursividad ni iteración
    def average(numbers: Seq[Double]): Double = numbers.reduce((x, y) => x+y) / numbers.length;
    
    val input: Seq[Double] = Seq(1, 2, 3, 4)
    
    println(s"${average(input)} should be 2.5")
}
