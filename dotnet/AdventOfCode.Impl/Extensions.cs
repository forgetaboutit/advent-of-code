using System.Text;
using System.Text.RegularExpressions;

namespace AdventOfCode.Impl;

public static class Extensions
{
    public static T Dump<T>(
        this T value,
        string? title = null)
    {
        var output = title is not null
            ? string.Concat(
                new string('=', title.Length),
                Environment.NewLine,
                title,
                Environment.NewLine,
                new string('=', title.Length),
                Environment.NewLine,
                Format(value))
            : Format(value);

        Console.WriteLine(output);

        return value;

        static string Format(
            T value)
            => value?.ToString()
               ?? "\u2205"; // Empty set symbol
    }

    public static TOut Let<TIn, TOut>(
        this TIn input,
        Func<TIn, TOut> f)
        => f(input);

    public static IEnumerable<IReadOnlyList<T>> Chunk<T>(
        this IEnumerable<T> source,
        uint chunksize)
    {
        if (chunksize > int.MaxValue)
        {
            throw new ArgumentOutOfRangeException(nameof(chunksize));
        }

        var currentChunk = new List<T>((int) chunksize);

        foreach (var item in source)
        {
            if (currentChunk.Count < chunksize)
            {
                currentChunk.Add(item);
            }
            else if (currentChunk.Count == chunksize)
            {
                yield return currentChunk;

                currentChunk =
                    new List<T>((int) chunksize)
                    {
                        item
                    };
            }
        }

        if (currentChunk.Any())
        {
            yield return currentChunk;
        }
    }

    /// <summary>
    ///     Lazily returns sliding windows over a sequence.
    ///     The algorithm makes sure to never return window slices smaller than the given size.
    ///     This also means that if the given size is larger than the number of items in the
    ///     sequence, no window slices are returned.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="source"></param>
    /// <param name="size"></param>
    /// <returns></returns>
    public static IEnumerable<T[]> Window<T>(
        this IEnumerable<T> source,
        int size)
    {
        if (size < 1)
        {
            throw new ArgumentOutOfRangeException(nameof(size), "must be > 0");
        }

        // Only get elements from the source when needed
        using (var e = source.GetEnumerator())
        {
            // ReSharper restore PossibleMultipleEnumeration

            // A queue allows efficient appends at the end and removals from the head
            var queue = new Queue<T>(size);

            while (true)
            {
                // Get elements lazily from the source
                while (queue.Count < size
                       && e.MoveNext())
                {
                    queue.Enqueue(e.Current);
                }

                // Abort when the window has reached the end of the sequence
                if (queue.Count < size)
                {
                    yield break;
                }

                // Use ToArray() instead of ToList() because Queue(Of T) implements it natively
                // as a fast array copy.  ToList() would require an enumerator and the extension
                // method to run, which is slower.
                yield return queue.ToArray();

                // Throw away the current head so the window moves forward
                if (queue.Any())
                {
                    queue.Dequeue();
                }
            }
        }
    }

    public static (T, T) Sort<T>(
        this T left,
        T right)
        where T : IComparable<T>
        => left.CompareTo(right) == 1
            ? (right, left)
            : (left, right);

    public static T Min<T>(
        this T left,
        T right)
        where T : IComparable<T>
        => left.CompareTo(right) == -1
            ? left
            : right;

    public static T Max<T>(
        this T left,
        T right)
        where T : IComparable<T>
        => left.CompareTo(right) == 1
            ? left
            : right;

    public static IEnumerable<TResult> MergeAdjacent<T, TKey, TResult>(
        this IEnumerable<T> source,
        Func<T, TKey> keySelector,
        Func<TKey, IEnumerable<T>, TResult> mergeSelector)
    {
        var isFirst = true;
        TKey? currentKey = default;
        List<T> currentGroup = new List<T>();
        var comparer = EqualityComparer<TKey>.Default;

        foreach (var item in source)
        {
            var currentItemKey = keySelector(item);

            if (isFirst || comparer.Equals(currentItemKey, currentKey))
            {
                currentGroup.Add(item);
                isFirst = false;
            }
            else
            {
                if (currentGroup.Any())
                {
                    yield return mergeSelector(currentKey!, currentGroup);
                }

                currentGroup = [item];
            }

            currentKey = currentItemKey;
        }

        if (currentGroup.Any())
        {
            yield return mergeSelector(currentKey!, currentGroup);
        }
    }

    public static IEnumerable<T> Scan<T, TState>(
        this IEnumerable<T> source,
        TState initialState,
        Func<TState, T, (TState newState, Maybe<T> element)> scanFn)
        => source.ScanProjected(
            initialState,
            scanFn);

    public static IEnumerable<TResult> ScanProjected<T, TState, TResult>(
        this IEnumerable<T> source,
        TState initialState,
        Func<TState, T, (TState newState, Maybe<TResult> element)> scanFn)
    {
        var state = initialState;

        foreach (var item in source)
        {
            var (newState, maybeElement) = scanFn(state, item);

            if (maybeElement.TryGet(out var element)
                && element is not null)
            {
                yield return element;
            }

            state = newState;
        }
    }

    public static IEnumerable<TResult> SelectByOccurrence<T, TKey, TResult>(
        this IEnumerable<T> source,
        Func<T, TKey> keySelector,
        Func<T, TResult> firstOccurrenceSelector,
        Func<T, TResult> duplicateOccurrenceSelector)
        => source
            .ScanProjected(
                new HashSet<TKey>(),
                (seen, item) =>
                {
                    var key = keySelector(item);

                    if (!seen.Add(key))
                    {
                        return (seen, Maybe.Some(duplicateOccurrenceSelector(item)));
                    }

                    return (seen, Maybe.Some(firstOccurrenceSelector(item)));
                });

    public static IEnumerable<T> Cycle<T>(
        this IEnumerable<T> source)
    {
        var buffer = source.ToArray();

        if (!buffer.Any())
        {
            yield break;
        }

        var index = 0;

        while (true)
        {
            yield return buffer[index];

            index = (index + 1) % buffer.Length;
        }
    }

    public static string JoinIntoString<T>(
        this IEnumerable<T> source,
        string separator)
        => string.Join(
            separator,
            source);

    public static string JoinWithNewline<T>(
        this IEnumerable<T> source,
        uint newlinesCount = 1)
        => source.JoinIntoString(
            Environment.NewLine.Repeat(newlinesCount));

    public static string[] SplitWithNewline(
        this string input)
        => input.Split(Environment.NewLine);

    public static string Repeat(
        this string s,
        uint count)
        => new StringBuilder(
                Environment.NewLine.Length * (int) count)
            .Insert(
                0,
                Environment.NewLine,
                (int) count)
            .ToString();

    public static List<long> ParseLongs(
        this string s)
        => new Regex(@"(-?\d+)")
            .Matches(s)
            .SelectMany(
                static mc => mc.Captures.Cast<Group>()
                    .SelectMany(
                        static g => g.Success
                            ? Enumerable.Repeat(long.Parse(g.Value), 1)
                            : Enumerable.Empty<long>()))
            .ToList();

    public static List<string> ParseAlphaNum(
        this string s)
        => new Regex(@"(\w+)")
            .Matches(s)
            .SelectMany(
                static mc => mc.Captures.Cast<Group>()
                    .SelectMany(
                        static g => g.Success
                            ? Enumerable.Repeat(g.Value, 1)
                            : Enumerable.Empty<string>()))
            .ToList();

    public static List<long> ParseLongsIgnoringSpaces(
        this string s)
        => new Regex(@"(\d+)")
            .Matches(s.Replace(" ", ""))
            .SelectMany(
                static mc => mc.Captures.Cast<Group>()
                    .SelectMany(
                        static g => g.Success
                            ? Enumerable.Repeat(long.Parse(g.Value), 1)
                            : Enumerable.Empty<long>()))
            .ToList();
}
