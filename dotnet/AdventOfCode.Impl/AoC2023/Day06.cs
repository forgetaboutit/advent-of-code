using System.Diagnostics;
using AdventOfCode.Impl.Data;

namespace AdventOfCode.Impl.AoC2023;

public static class Day06
{
    public static AoCResult<int, int> Solve(
        string input = Input)
        => AoCResult.Create(
            Races
                .ParseMultiple(input)
                .RaceData
                .Aggregate(
                    1,
                    static (acc, race) => race.CountPossibleWins() * acc),
            Race
                .ParseSingle(input)
                .CountPossibleWins());

    private sealed record Races(
        List<Race> RaceData)
    {
        public static Races ParseMultiple(
            string input)
        {
            if (input.SplitWithNewline() is [var times, var distances])
            {
                return new Races(
                    times
                        .ParseLongs()
                        .Zip(
                            distances.ParseLongs(),
                            static (fst, snd) => new Race(fst, snd))
                        .ToList());
            }

            throw new UnreachableException();
        }
    }

    private sealed record Race(
        long Time,
        long Distance)
    {
        public int CountPossibleWins()
            => Enumerable
                .Range(1, (int) Time)
                .Select(holdInSeconds => holdInSeconds * (Time - holdInSeconds))
                .Count(distance => distance > Distance);

        public static Race ParseSingle(
            string input)
        {
            if (input.SplitWithNewline() is [var times, var distances]
                && times.ParseLongsIgnoringSpaces() is [var time]
                && distances.ParseLongsIgnoringSpaces() is [var distance])
            {
                return new Race(
                    time,
                    distance);
            }

            throw new UnreachableException();
        }
    }

    public const string Ex =
        """
        Time:      7  15   30
        Distance:  9  40  200
        """;

    public const string Input =
        """
        Time:        58     99     64     69
        Distance:   478   2232   1019   1071
        """;
}
