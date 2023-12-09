namespace AdventOfCode.Impl;

public static partial class Algorithms
{
    // The cache for prime numbers is thread local to avoid the complexities of
    // synchronization while keeping things fast enough.
    [ThreadStatic]
    private static SortedSet<long>? _primeCache;

    [ThreadStatic]
    private static long? _lastNumberTestedForPrimality;

    public static IEnumerable<long> GeneratePrimes()
    {
        if (_primeCache is not null)
        {
            foreach (var prime in _primeCache)
            {
                yield return prime;
            }
        }

        _primeCache ??= [];

        var startingCandidate = _lastNumberTestedForPrimality ?? 2;

        for (var candidate = startingCandidate;; candidate++)
        {
            _lastNumberTestedForPrimality = candidate;

            if (IsPrime(candidate))
            {
                _primeCache.Add(candidate);

                yield return candidate;
            }
        }
    }

    public static bool IsPrime(
        long value)
    {
        // If primes were generated previously, check the cache. This should be a lot
        // faster than sieving, especially for larger numbers.
        if (_primeCache is not null
            && value < _lastNumberTestedForPrimality)
        {
            return _primeCache.Contains(value);
        }

        var upperLimit = (long) Math.Sqrt(value) + 1;

        for (var i = 2; i <= upperLimit; i++)
        {
            if (value != i
                && value % i == 0)
            {
                return false;
            }
        }

        return true;
    }
}
