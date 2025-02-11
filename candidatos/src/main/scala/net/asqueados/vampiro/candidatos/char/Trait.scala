/**
  *  i.e. attributes, skills, concept, anything we have in the character
  */
trait Trait[+A] {
    def name(): String
    def value(): A

    override def toString() = s"$name: $value"
}
