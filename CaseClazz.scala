object CaseClazz extends App {
    case class Person (name: String, isFemale: Boolean)

    var person = Person("pepe", false)

    println(person.name)

    // produces error: reassignment to val
    // person.name = "Juan"

    case class Rectangle(base: Double, height: Double) {
        def area(): Double = base * height
    }

    println(Rectangle(2, 3).area())
}