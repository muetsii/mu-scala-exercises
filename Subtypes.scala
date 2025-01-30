object Subtypes extends App {
    class Animal (_name: String) {
        val name = _name
        val fitness = 1
        override def toString() = name
    }
    class Dog(_name: String) extends Animal(_name) {
        override val fitness = 3
    }
    class Cat(_name: String) extends Animal(_name) {
        override val fitness = 2
    }

    def select[A <: Animal](a1: A, a2: A): A = {
        if (a1.fitness > a2.fitness) a1
        else a2
    }

    val toby = new Dog("toby")
    val rufo = new Dog("rufo")
    val misu = new Cat("misu")
    val raya = new Cat("raya")

    println(select(toby, rufo))
    println(select(misu, raya))
    println(select(rufo, raya))
}