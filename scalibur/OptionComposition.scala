/*

Imaginemos que tenemos una fuente de datos que nos devuelve el nombre, apellido y edad de individuos. 

Pero los datos pueden estar incompletos.

Nosotros queremos crear individuos a partir de esos datos sólo si tenemos los datos completos.

*/

object OptionComposition extends App {

    case class Person(firstName: String, lastName: String, age: Int)
    
    def createPerson(firstName: Option[String], lastName: Option[String], age: Option[Int]): Option[Person] = ???
    
    val person1 = createPerson(Some("Pepe"), Some("Pelas"), None)
    val person2 = createPerson(Some("Pepe"), None, Some(56))
    val person3 = createPerson(None, Some("Pelas"), Some(56))
    val person4 = createPerson(Some("Pepe"), Some("Pelas"), Some(56))

    println(s"$person1 should be None")
    println(s"$person2 should be None")
    println(s"$person3 should be None")
    println(s"$person4 should be Some(Person(Pepe,Pelas,56))")

}
