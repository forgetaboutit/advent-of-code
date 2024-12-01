import kotlin.math.abs

fun main() {
    val input = InputLoader().load("AoC2024D1")
    val intLists =
        input.split("\n").map { it.split(Regex("\\s+")).map { i -> i.toInt() } }
    val partOne = intLists.map { it.toPair() }.transpose().map { it.sorted() }.zip()
        .sumOf { abs(it.first - it.second) }
    val partTwo = intLists.sumOf { it[0] * intLists.count { i -> i[1] == it[0] } }

    println(partOne)
    println(partTwo)
}
