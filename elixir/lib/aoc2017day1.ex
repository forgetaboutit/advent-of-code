defmodule Aoc2017Day1 do
  @spec part1() :: integer()
  def part1 do
    read_digits()
    |> sum_compared_to_offset(1)
  end

  @spec part2() :: integer()
  def part2 do
    digits = read_digits()
    middle_point = length(digits) / 2 |> trunc()

    digits
    |> sum_compared_to_offset(middle_point)
  end

  @spec sum_compared_to_offset(charlist(), integer()) :: integer()
  defp sum_compared_to_offset(digits, offset) do
    offsetted = digits
    |> Stream.cycle()
    |> Stream.drop(offset)

    digits
    |> Stream.map(fn i -> i end)
    |> Stream.zip_with(offsetted, fn l, r ->
      if l === r do
        l
      else
        0
      end
    end)
    |> Enum.sum()
  end

  @spec read_digits() :: charlist()
  defp read_digits() do
    {:ok, text} = File.read("./inputs/2017day1.txt")

    text
    |> String.trim()
    |> String.to_charlist()
    |> Enum.map(&char_to_digit/1)
  end

  # ASCII digit char to actual digit value
  defp char_to_digit(c), do: c - 48
end
