using System.Collections;

namespace AdventOfCode.Impl.Data;

public enum RangeType
{
    SingleCell,
    SameRow,
    SameColumn,
    Diagonal
}

public sealed class Range2D(
    Position2D start,
    Position2D end)
    : IEnumerable<Position2D>
{
    public Position2D Start { get; } = start;

    public Position2D End { get; } = end;

    public RangeType Type { get; } =
        FindType(start, end)
        ?? throw new NotSupportedException(
            "Only straight or diagonal ranges are supported.");

    public bool IsAdjacentDiagonally(
        Position2D other)
    {
        var isAdjacentDiagonally = this.Any(pos => pos.IsAdjacentDiagonally(other));

        return isAdjacentDiagonally;
    }

    IEnumerator IEnumerable.GetEnumerator()
        => GetEnumerator();

    public IEnumerator<Position2D> GetEnumerator()
    {
        return (Type switch
        {
            RangeType.SingleCell => EnumerateSingleCell(),
            RangeType.SameRow => EnumerateSameRow(),
            RangeType.SameColumn => EnumerateSameColumn(),
            RangeType.Diagonal => EnumerateSameDiagonal(),
            _ => throw new ArgumentOutOfRangeException()
        }).GetEnumerator();

        IEnumerable<Position2D> EnumerateSingleCell()
            => Enumerable.Repeat(Start, 1);

        IEnumerable<Position2D> EnumerateSameRow()
        {
            var (min, max) = Start.X.Sort(End.X);

            for (var i = min; i <= max; i++)
            {
                yield return Start with
                             {
                                 X = i
                             };
            }
        }

        IEnumerable<Position2D> EnumerateSameColumn()
        {
            var (min, max) = Start.Y.Sort(End.Y);

            for (var i = min; i <= max; i++)
            {
                yield return Start with
                             {
                                 Y = i
                             };
            }
        }

        IEnumerable<Position2D> EnumerateSameDiagonal()
            => throw new NotImplementedException(
                "Diagonal traveral is not implemented yet");
    }

    private static RangeType? FindType(
        Position2D start,
        Position2D end)
        => (start.DiffX(end), start.DiffY(end)) switch
        {
            (0, 0) => RangeType.SingleCell,
            (var x, 0) when x != 0 => RangeType.SameRow,
            (0, var y) when y != 0 => RangeType.SameColumn,
            var (x, y) when Math.Abs(x) == Math.Abs(y) => RangeType.Diagonal,
            _ => null
        };
}
