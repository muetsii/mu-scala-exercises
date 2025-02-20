object Covariance extends App {

    class Fruit
    class Apple extends Fruit
    class Banana extends Fruit
    class Citrus extends Fruit
    class Tangelo extends Citrus
    class Orange extends Citrus
    class NavelOrange extends Orange //Creating a subtype to prove a point

    // Contravariance
    class MyContraContainer[-A](a: A)(implicit manifest: scala.reflect.Manifest[A]) { //Can't receive a val because it would be in a covariant position
        def contents = manifest.runtimeClass.getSimpleName
    }

    val citrusBasket: MyContraContainer[Citrus] = new MyContraContainer[Citrus](new Orange)
    println(citrusBasket.contents)
    val orangeBasket: MyContraContainer[Orange] =
        new MyContraContainer[Citrus](new Tangelo)
    println(orangeBasket.contents)
    val tangeloBasket: MyContraContainer[Tangelo] =
        new MyContraContainer[Citrus](new Orange)
    println(tangeloBasket.contents)
    val bananaBasket: MyContraContainer[Banana] = new MyContraContainer[Fruit](new Apple)
    println(bananaBasket.contents)
}