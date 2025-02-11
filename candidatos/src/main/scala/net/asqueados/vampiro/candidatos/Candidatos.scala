object Candidatos extends App {
    val printer = new PrinterScreen()
    val formatter = new CharacterFormatterLines()
    val generator = new CharacterGenerator()

    printer.print(formatter.format(generator.generate()))
}
