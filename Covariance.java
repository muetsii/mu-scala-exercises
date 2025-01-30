class Covariance {
    static abstract class Mammal {
        abstract String species();
        void hello() {
            System.out.println("Hello, I am " + this.species());
        }
    }
    static class Zebra extends Mammal {
        String species() { return "Zebra"; };
    }
    static class Giraffe extends Mammal {
        String species() { return "Giraffe"; };
    }

    public static void main(String args[]) { 
        Zebra[] zebras = new Zebra[]{ new Zebra() };  // Array containing 1 `Zebra`
        Mammal[] mammals = zebras;      // Allowed because arrays are covariant in Java
        mammals[0] = new Giraffe();     // Allowed because a `Giraffe` is a subtype of `Mammal`
        Zebra zebra = zebras[0];        // Get the first `Zebra` … which is actually a `Giraffe`!

        zebra.hello();
    }
}