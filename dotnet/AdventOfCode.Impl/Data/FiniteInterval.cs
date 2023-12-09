namespace AdventOfCode.Impl.Data;

public sealed record FiniteInterval(
    long Start,
    long EndInclusive)
{
    public long EndInclusive { get; } = Start <= EndInclusive
        ? EndInclusive
        : throw new ArgumentOutOfRangeException(
            nameof(EndInclusive));
}
