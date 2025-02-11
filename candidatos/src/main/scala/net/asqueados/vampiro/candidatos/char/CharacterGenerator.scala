class CharacterGenerator {
    def generate(): Character = {
        val traitGroups = List(
            new GroupIdentity()
        )

        new Character("Pepo", traitGroups)
    }
}
