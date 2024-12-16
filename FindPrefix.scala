import scala.annotation.tailrec

object FindPrefix extends App {

    def findPrefix(sentence: String, searchWord: String, acc: Int = 0): Int = {
        @tailrec
        def findPrefixRec(words: List[String], searchWord: String, acc: Int): Int = words match {
            case Nil => -1
            case head :: tail if head.startsWith(searchWord) => acc
            case _ :: tail => findPrefixRec(tail, searchWord, acc + 1)
        }
        
        findPrefixRec(sentence.split(" ").toList, searchWord, 1)
        
    }
    
    val ex1 = findPrefix("i love eating burger", "burg")
    val ex2 = findPrefix("this problem is an easy problem", "pro")
    val ex3 = findPrefix("i am tired", "you")
    
    println(s"$ex1 should be 4")
    println(s"$ex2 should be 2")
    println(s"$ex3 should be -1")
    
}
