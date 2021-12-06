<Query Kind="Program" />

private const int MaxTimer = 8;

private static long[] ParseInput(
    string input)
{
    var numbers = input.
        Split(",").
        Select(int.Parse).
        GroupBy(i => i).
        Select(g => (g.Key, g.Count())).
        OrderBy(t => t.Key).
        ToList();

    // +1 slot for timer value `0`
    var state = new long[MaxTimer + 1];

    for (var i = 1; i <= MaxTimer; i++)
    {
        state[i] = numbers.
            FirstOrDefault(t => t.Key == i).Item2;
    }

    return state;
}

private static long[] RunSimulation(
    long[] startingState,
    int days)
{
    var currentState = startingState;

    for (var day = 0; day < days; day++)
    {
        var nextState = new long[MaxTimer + 1];
        var zeroes = currentState[0];

        for (var i = 1; i <= MaxTimer; i++)
        {
            nextState[i - 1] = currentState[i];
        }

        // Give birth, reset timers
        nextState[6] += zeroes;
        // Make some babies
        nextState[8] = zeroes;

        currentState = nextState;
    }

    return currentState;
}

public static long CountAfterDays(
    string input,
    int days)
    => RunSimulation(
            ParseInput(input),
            days).
        Sum();

void Main()
{
    const string input = @"3,5,2,5,4,3,2,2,3,5,2,3,2,2,2,2,3,5,3,5,5,2,2,3,4,2,3,5,5,3,3,5,2,4,5,4,3,5,3,2,5,4,1,1,1,5,1,4,1,4,3,5,2,3,2,2,2,5,2,1,2,2,2,2,3,4,5,2,5,4,1,3,1,5,5,5,3,5,3,1,5,4,2,5,3,3,5,5,5,3,2,2,1,1,3,2,1,2,2,4,3,4,1,3,4,1,2,2,4,1,3,1,4,3,3,1,2,3,1,3,4,1,1,2,5,1,2,1,2,4,1,3,2,1,1,2,4,3,5,1,3,2,1,3,2,3,4,5,5,4,1,3,4,1,2,3,5,2,3,5,2,1,1,5,5,4,4,4,5,3,3,2,5,4,4,1,5,1,5,5,5,2,2,1,2,4,5,1,2,1,4,5,4,2,4,3,2,5,2,2,1,4,3,5,4,2,1,1,5,1,4,5,1,2,5,5,1,4,1,1,4,5,2,5,3,1,4,5,2,1,3,1,3,3,5,5,1,4,1,3,2,2,3,5,4,3,2,5,1,1,1,2,2,5,3,4,2,1,3,2,5,3,2,2,3,5,2,1,4,5,4,4,5,5,3,3,5,4,5,5,4,3,5,3,5,3,1,3,2,2,1,4,4,5,2,2,4,2,1,4";

    CountAfterDays(input, 80).Dump();
    CountAfterDays(input, 256).Dump();
}
