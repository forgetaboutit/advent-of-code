class InputLoader {
    fun load(name: String): String {
        return (object {}).javaClass.getResource("/${name}")!!.readText()
    }
}