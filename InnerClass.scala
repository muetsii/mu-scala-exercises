object InnerClass extends App {
    class Hengeyokai(name: String, race: String, breed: String, auspice: String, rank: Int = 1) {
        require(rank >= 0)
        require(rank <= 6)

        def describe(): String = {
            s"$name: $breed $auspice $race"
        }
    }

    val dragona = new Hengeyokai("Dòng Chảy", "Zhong Long", "Homid", "Pillar")

    println(dragona.describe())

    try {
        val floki = new Hengeyokai("Floki", "Garra Roja", "Lupus", "Ahroun", 23)
    } catch {
        case e : Exception => println("El puto Floki es demasiado poderoso")
    }
}
