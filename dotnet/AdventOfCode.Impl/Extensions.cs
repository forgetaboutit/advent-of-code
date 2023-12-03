using System.Text;

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

    public static IEnumerable<T[]> Window<T>(
        this IEnumerable<T> source,
        int size)
    {
        using var e = source.GetEnumerator();

        var window = new T[size];
        var currentWindowIndex = 0;

        while (true)
        {
            while (window.Length < size
                   && e.MoveNext())
            {
                window[currentWindowIndex] = e.Current;
                currentWindowIndex = (currentWindowIndex + 1) % size;
            }

            // We don't have enough items left, so exit
            if (window.Length < size)
            {
                yield break;
            }

            yield return window;
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
}
