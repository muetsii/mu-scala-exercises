// DOES NOT WORK
object TupleMap extends App {
    case class Person(name: String, age: Int)

    val person = ("Pepe", 25).map(p => Person(t._1, t._2))

    println(person)
}