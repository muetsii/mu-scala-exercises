object Candidatos extends App {
    val printer = new PrinterFile()
    val formatter = new CharacterFormatterLines()
    val generator = new CharacterGenerator()

    printer.print(formatter.format(generator.generate()))
}
