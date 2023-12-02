using System.Buffers;
using System.Diagnostics;
using System.Security.Cryptography;
using System.Text;
using AdventOfCode.Impl.Data;

namespace AdventOfCode.Impl.AoC2015;

public static class Day04
{
    public static AoCResult<int, int> SolveWithArrays()
    {
        var found5 = false;
        int with5Zeroes = default;

        for (var i = 0; i < int.MaxValue; i++)
        {
            var value = string.Concat(Input, i.ToString());
            var hashInput = Encoding.ASCII.GetBytes(value);
            var hash = MD5.HashData(hashInput);

            if (!found5
                && hash is [0, 0, <= 9, ..])
            {
                with5Zeroes = i;

                found5 = true;
            }

            if (hash is [0, 0, 0, ..])
            {
                return AoCResult.Create(
                    with5Zeroes,
                    i);
            }
        }

        throw new UnreachableException();
    }

    public static AoCResult<int, int> SolveWithSpans()
    {
        var found5 = false;
        int with5Zeroes = default;
        var hashInputBuffer = ArrayPool<byte>.Shared.Rent(1024);
        var hashOutputBuffer = ArrayPool<byte>.Shared.Rent(16);
        var offset = Encoding.ASCII.GetBytes(Input.AsSpan(), hashInputBuffer);
        var numberDestination = hashInputBuffer.AsSpan()[offset..];

        for (var i = 0; i < int.MaxValue; i++)
        {
            i.TryFormat(numberDestination, out var bytesWritten);

            MD5.TryHashData(
                hashInputBuffer.AsSpan()[..(offset + bytesWritten)],
                hashOutputBuffer,
                out _);

            if (!found5
                && hashOutputBuffer is [0, 0, <= 9, ..])
            {
                with5Zeroes = i;
                found5 = true;
            }

            if (hashOutputBuffer is [0, 0, 0, ..])
            {
                ArrayPool<byte>.Shared.Return(hashInputBuffer);
                ArrayPool<byte>.Shared.Return(hashOutputBuffer);

                return AoCResult.Create(
                    with5Zeroes,
                    i);
            }
        }

        throw new UnreachableException();
    }

    private const string Input = "iwrupvqb";
}
