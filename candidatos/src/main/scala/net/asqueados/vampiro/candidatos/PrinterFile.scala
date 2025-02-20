class PrinterFile(fileName: String = "out.out", basePath: String = System.getProperty("user.dir")) extends Printer {
    val path = os.Path(basePath) / os.Path(fileName)

    def print(text: String): Unit = {
        os.write.append(path, text)
    }
}
