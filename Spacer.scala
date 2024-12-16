object Spacer {
  def subString(str: String, spaces: List[Int], index: Int): String = {
    if (index + 1 >= spaces.length)
      return str.substring(spaces(index))

    str.substring(spaces(index), spaces(index + 1))
  }

  /**
    *  @param s string
    *  @param spaces array of indices where to insert spaces
    */
  def addSpaces(s: String, spaces: Array[Int]): String = {
    val spaceIndexes = (List(0) ++ spaces)

    //spaceIndexes.zipWithIndex.map((t) => s.substring(t._1, spaceIndexes(t._2 + 1))).mkString(" ")
    spaceIndexes.zipWithIndex.map((t) => this.subString(s, spaceIndexes, t._2)).mkString(" ")

    // ...
  }
}
