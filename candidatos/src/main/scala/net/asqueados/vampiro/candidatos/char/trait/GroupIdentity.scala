class GroupIdentity extends TraitGroup[Identity] {
    def name() = "Identity"
    def traits() = Map(
        "Naturaleza" -> new NatureDemeanor("Naturaleza"),
        "Conducta" -> new NatureDemeanor("Conducta")
    )
}
