// we have a list containing lists of values (any)
// we want a list of strings (comma separated values)

object ListAnyMapListString extends App {
    val listsAny = List[List[Any]](
        List[Any]("Pepe", 20, 1.0),
        List[Any]("Paco", 30, 3.0)
    )

    val textLines: List[String] = listsAny.map(row => row.mkString(","))
    // expected: List[String]("Pepe,20,1.0","Paco,30.3.0")

    println(textLines)
}
