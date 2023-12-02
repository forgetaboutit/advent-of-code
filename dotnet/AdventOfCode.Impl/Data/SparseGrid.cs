using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace AdventOfCode.Impl.Data;

public readonly record struct Cell<T>(
    Position2D Position,
    T Value);

public sealed class SparseGrid<T>
    : IEnumerable<Cell<T>>
{
    private readonly Dictionary<Position2D, T> _cells = new();

    public void Set(
        Position2D position,
        T value)
        => _cells[position] = value;

    public void AddOrUpdate(
        Position2D position,
        T value,
        Func<Cell<T>, T> onUpdate)
    {
        if (_cells.TryGetValue(
                position,
                out var cell))
        {
            _cells[position] = onUpdate(
                new Cell<T>(
                    position,
                    cell!));
        }
        else
        {
            _cells.Add(
                position,
                value);
        }
    }

    public bool TryGetValue(
        Position2D position,
        [NotNullWhen(true)] out T? cell)
        => _cells.TryGetValue(
            position,
            out cell);

    public bool TryGetCell(
        Position2D position,
        [NotNullWhen(true)] out Cell<T>? cell)
    {
        if (_cells.TryGetValue(
                position,
                out var value))
        {
            cell = new Cell<T>(
                position,
                value);

            return true;
        }

        cell = default;

        return false;
    }

    IEnumerator IEnumerable.GetEnumerator()
        => GetEnumerator();

    public IEnumerator<Cell<T>> GetEnumerator()
    {
        return Enumerate().GetEnumerator();

        IEnumerable<Cell<T>> Enumerate()
            => _cells
                .Select(
                    static kvp => new Cell<T>(
                        kvp.Key,
                        kvp.Value));
    }
}
