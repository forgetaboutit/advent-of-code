namespace AdventOfCode.Impl.Data;

/// <summary>
/// Position on a 2D plane of a cartesian coordinate system.
/// </summary>
/// <param name="X">
///     The x component in a cartesian coordinate system. +x means right, -x means left.
/// </param>
/// <param name="Y">
///     The y component in a cartesian coordinate system. +y means up, -y means down.
/// </param>
public readonly record struct Position2D(
    int X,
    int Y)
{
    public static readonly Position2D Zero = new(0, 0);

    public Position2D Up()
        => this with { X = X + 1 };

    public Position2D Down()
        => this with { X = X - 1 };

    public Position2D Left()
        => this with { Y = Y - 1 };

    public Position2D Right()
        => this with { Y = Y + 1 };

    public Position2D UpLeft()
        => new(X - 1, Y - 1);

    public Position2D UpRight()
        => new(X + 1, Y - 1);

    public Position2D DownLeft()
        => new(X - 1, Y - 1);

    public Position2D DownRight()
        => new(X + 1, Y - 1);

    public int DiffX(
        Position2D other)
        => other.X - X;

    public int DiffY(
        Position2D other)
        => other.Y - Y;

    public bool IsAdjacentDiagonally(
        Position2D other)
        => (Math.Abs(DiffX(other)), Math.Abs(DiffY(other)))
            switch
            {
                (0, 1) => true,
                (1, 0) => true,
                (1, 1) => true,
                _ => false
            };

    public override string ToString()
        => $"(X: {X}, Y: {Y})";
}
