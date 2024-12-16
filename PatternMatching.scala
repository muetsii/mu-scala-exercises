// Pattern matching



object PatternMatching extends App {

    val list = List(1, 2, 3, 4, 5)

    println("* Es como un switch, pero con esteroides")
    list.foreach { e =>
        e match {
            case 1 => println("uno")
            case 2 => println("dos")
            case 3 => println("tres")
            case _ => println("ya no sé contar más")
        }
    }
    
    println("\n* Condiciones")
    list.foreach { e =>
        e match {
            case i if i %2 == 0 => println(s"$i es par")
            case i => println(s"$i es impar")
        }
    }
    
    println("\n* Comprobar tipos")
    val mixedList = List(1, "dos", 3)
    
    mixedList.foreach { e =>
        e match {
            case i: Int => println(s"$i es un entero")
            case s: String => println(s"$s es una cadena")
        }
    }
    
    println("\n* Extraer miembros")
    case class Person(name: String, gender: String, age: Int)
    val family = List(
        Person("Pepe", "male", 45),
        Person("Pepa", "female", 25),
        Person("Pepito", "male", 13)
    )
    
    family.foreach { e =>
        e match {
            case Person(name, _, age) =>
                val adjective = if(age >= 18) // Esto se podría haber comprobado también en el mismo case
                        "adult"
                    else
                        "minor"
                println(s"$name is $adjective")
                    
        }
    }
    
    println("\n* Una de las formas de comprobar Option")
    val optionList = List(Some(1), None, Some(3), None)
    optionList.foreach { e =>
        e match {
            case Some(i) => println(s"$i está definido")
            case None => println(s"no está definido") // Podría haber valido case _
        }
    }
        
    
    
    println("\n* Expresiones regulares")
    val email = "([^@]+)@(.*)".r
    val url = "([^\\.]+)\\.([^\\.]+\\..*)".r
    
    val netThings = List("elon.musk@tesla.com", "www.tesla.com", "Pig")
    
    netThings.foreach {
        // En realidad, puedes hacer pattern-matching dentro de un foreach, map, etc 
        // no necesitas escribir match
        case email(name, domain) => println(s"email: $name at $domain")
        case url(subdomain, domain) => println(s"url: $subdomain in $domain")  
        case other => println(s"I don't know what is $other") 
    }
    
    println("\n* Listas")
    // Esto es lo que se suele usar en recursividad
    val listOfLists = List(
        List(1, 2, 3, 4, 5),
        List(1),
        List()
    )
    
    listOfLists.foreach {
        case Nil => println("list is empty") // Nil significa lista vacía
        case head :: Nil => println(s"list only has one element: $head")
        case head :: tail => println(s"$head is the first element and the rest are ${tail.mkString(",")}")
    }
    
    println("\n* Recursividad")
    def sumRec(numbers: List[Int], acc: Int = 0): Int = numbers match {
        case Nil => acc
        case head :: tail => sumRec(tail, acc+head)
    }
    
    println(sumRec(List(1, 2, 3, 4)))
    
}
