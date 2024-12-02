import kotlin.math.abs

fun main() {
    val input = InputLoader().load("AoC2024D2")
    val cells =
        input.split("\n").map { it.split(Regex("\\s+")).map { i -> i.toInt() } }

    val partOne = cells.count { isSafe(it) }
    val partTwo = cells.count { isSafeWithoutOne(it) }

    println(partOne)
    println(partTwo)
}

enum class Direction { Unknown, Asc, Desc }

fun isSafe(row: List<Int>): Boolean {
    var previousDirection = Direction.Unknown

    for (chunk in row.windowed(2)) {
        val delta = chunk[1] - chunk[0]

        if (abs(delta) > 3 || delta == 0) {
            return false
        }

        val currentDirection = if (delta > 0) Direction.Asc else Direction.Desc

        if (previousDirection == Direction.Unknown) {
            previousDirection = currentDirection
        } else if (previousDirection != currentDirection) {
            return false
        }
    }

    return true
}

fun isSafeWithoutOne(row: List<Int>): Boolean {
    if (isSafe(row)) {
        return true
    }

    for (index in row.indices) {
        val copy = row.toMutableList()
        copy.removeAt(index)

        if (isSafe(copy)) {
            return true
        }
    }

    return false
}