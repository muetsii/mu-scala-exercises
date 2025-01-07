object Operators extends App {
    val RNG = new scala.util.Random

    class Person (n: String, f: Boolean = false) {
        val name = n
        val isFemale = f

        def describe(): String = name

        def isHuman(): Boolean = true

        def * (mate: Person): Person = {
            require(mate.isFemale != this.isFemale, "cannot mate with same sex")
            val gender = RNG.nextBoolean
            if (mate.isHuman()) new Person(this.mixName(mate.name), gender)
            else mate * this
        }

        def mixName(anotherName: String): String = {
            this.name.slice(0, this.name.length / 2) + anotherName.slice(anotherName.length / 2, anotherName.length)
        }
    }

    class Hengeyokai(n: String, f: Boolean = false, r: String, b: String, a: String, rnk: Int = 1) extends Person (n, f) {
        require(rnk >= 0)
        require(rnk <= 6)

        val race = r
        val breed = b
        val auspice = a
        val rank = rnk

        // def this(n: String, r: String, b: String, a: String, rnk: Int = 1) {
        //     this(n, false, r, b, a, rnk)
        // }

        override def isHuman(): Boolean = false

        override def describe(): String = {
            s"$name: $breed $auspice $race"
        }

        override def * (mate: Person): Person = {
            require(mate.isFemale != this.isFemale, "cannot mate with same sex")
            val gender = RNG.nextBoolean

            if (mate.isHuman()) new Hengeyokai(this.mixName(mate.name), gender, this.race, this.breed, "", 0)
            else {
                require(this.race == mate.asInstanceOf[Hengeyokai].race, s"cannot mix ${this.race} with  ${mate.asInstanceOf[Hengeyokai].race}")
                new Hengeyokai(this.mixName(mate.name), gender, this.race, "Metis", "", 0)
            }
        }
    }

    val dragona = new Hengeyokai("Dòng Chảy", true, "Zhong Long", "Homid", "Pillar")
    val dragono = new Hengeyokai("Trong", false,  "Zhong Long", "Draco", "Nam Hsia")

    val floki = new Hengeyokai("El Puto Floki", false, "Garou", "Lupus", "Ahroun", 6)
    val nut =   new Hengeyokai("Nut",           true,  "Garou", "Lupus", "Ahroun", 2)

    val lorena = new Person("Lorena", true)
    val rastas = new Person("Rastas", true)
    val mario  = new Person("Mario",  false)

    val everyone = List(dragona, dragono, floki, nut, lorena, rastas, mario)

    for (one <- everyone; other <- everyone) {
        // printn(s" $one.name <3 $other.name")
        try {
            if (one != other) {
                val child = one * other
                println(child.describe())
            }
        } catch {
            case e: Exception => println(s"Cannot mate ${one.name} with ${other.name} : ${e.getMessage()}")
        }
    }
}
