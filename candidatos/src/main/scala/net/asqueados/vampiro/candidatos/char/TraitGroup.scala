/**
  *  i.e. attributes, skills, concept, anything we have in the character
  */
trait TraitGroup [+A <: Trait[Any]] {
    def name(): String
    def traits(): Map[String, A]
    // def generateTraits(): Unit
}
