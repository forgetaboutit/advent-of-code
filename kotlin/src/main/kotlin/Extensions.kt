fun <T> Collection<Pair<T, T>>.transpose(): Pair<Collection<T>, Collection<T>> {
    val left = ArrayList<T>(this.size)
    val right = ArrayList<T>(this.size)

    for (element in this) {
        left.add(element.first)
        right.add(element.second)
    }

    return Pair(left, right)
}

fun <T, E> Pair<T, T>.map(f: (T) -> E): Pair<E, E> = Pair(
    this.first.run(f), this.second.run(f)
)

fun <T> Pair<Iterable<T>, Iterable<T>>.zip(): List<Pair<T, T>> = this.first.zip(
    this.second
) { l: T, r: T -> Pair(l, r) }

fun <T> Iterable<T>.toPair(): Pair<T, T> {
    val two = this.take(2)
    return Pair(two[0], two[1])
}
