namespace AdventOfCode.Impl;

public static partial class Algorithms
{
    public static long LeastCommonMultiple(
        params long[] values)
        => values
            .Select(PrimeFactors)
            .SelectMany(
                static factors => factors
                    .GroupBy(static factor => factor)
                    .Select(static g => (Factor: g.Key, Occurrences: g.Count())))
            .GroupBy(static t => t.Factor)
            .Select(
                static g => (long) Math.Pow(
                    g.Key,
                    g.Max(static t => t.Occurrences)))
            .Aggregate(
                1L,
                static (acc, factorPower) => acc * factorPower);

    public static List<long> PrimeFactors(
        long value)
    {
        var factors = new List<long>();
        var rest = value;

        while (rest > 1)
        {
            foreach (var prime in GeneratePrimes())
            {
                var quotient = Math.DivRem(rest, prime, out var remainder);

                if (remainder == 0)
                {
                    factors.Add(prime);
                    rest = quotient;

                    break;
                }
            }
        }

        return factors;
    }
}
