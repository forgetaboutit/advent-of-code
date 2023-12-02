namespace AdventOfCode.Impl.Data;

public readonly record struct AoCResult<TFirst, TSecond>(
    TFirst FirstResult,
    TSecond SecondResult)
{
    public void Dump()
    {
        FirstResult.Dump("First result");
        SecondResult.Dump("Second result");
    }
}

public static class AoCResult
{
    public static AoCResult<TFirst, TSecond> Create<TFirst, TSecond>(
        TFirst first,
        TSecond second)
        => new(first, second);
}
