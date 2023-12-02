using AdventOfCode.Impl.AoC2015;
using AdventOfCode.Impl.Data;
using BenchmarkDotNet.Attributes;

namespace AdventOfCode.Launcher.Benchmarks;

[MemoryDiagnoser]
public class Benchmarks2015Day01
{
    [Benchmark]
    public AoCResult<int, int> WithByteArray()
        => Day04.SolveWithArrays();

    [Benchmark]
    public AoCResult<int, int> WithSpans()
        => Day04.SolveWithSpans();
}
