trait CharacterFormatter {
    def begin(): String
    def end(): String
    def separator(): String

    def group(traitGroup: TraitGroup[Trait[Any]]): String = {
        separator() + (
            traitGroup.traits.values.map(t => t.toString).reduceLeft{_ + separator() + _}
        )
    }

    def format(char: Character): String = {
        begin() + (
            char.traitGroups.map(tg => group(tg))
        ) + end()
    }
}
