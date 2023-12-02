namespace AdventOfCode.Impl.Data;

/// <summary>
/// Position on a 2D plane of a cartesian coordinate system.
/// </summary>
/// <param name="Y">
/// The y component in a cartesian coordinate system. +y means up, -y means down.
/// </param>
/// <param name="X">
/// The x component in a cartesian coordinate system. +x means right, -x means left.
/// </param>
public readonly record struct Position2D(
    int Y,
    int X)
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
        => new(
            Y - 1,
            X - 1);

    public Position2D UpRight()
        => new(
            Y - 1,
            X + 1);

    public Position2D DownLeft()
        => new(
            Y - 1,
            X - 1);

    public Position2D DownRight()
        => new(
            Y - 1,
            X + 1);

    public override string ToString()
        => $"(Y: {Y}, X: {X})";
}
