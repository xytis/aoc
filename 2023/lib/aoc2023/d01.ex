defmodule AOC2023.D01 do
  @moduledoc """
  Documentation for `AOC2023.D01`.
  """

  def solve() do
    IO.puts("Day 1")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d01.txt")
  end

  @spec parse(binary()) :: [1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9]
  @doc """
  Parse a line of input into a list of all occurences of numbers.

  ## Examples

      iex> AOC2023.D01.parse("zoneight234")
      [1, 8, 2, 3, 4]
  """
  def parse(binary)

  # numeric
  for {string_number, number} <- [
        {"1", 1},
        {"2", 2},
        {"3", 3},
        {"4", 4},
        {"5", 5},
        {"6", 6},
        {"7", 7},
        {"8", 8},
        {"9", 9}
      ] do
    def parse(<<unquote(string_number), rest::binary>>), do: [unquote(number) | parse(rest)]
  end

  # word like
  for {string_number, number} <- [
        {"one", 1},
        {"two", 2},
        {"three", 3},
        {"four", 4},
        {"five", 5},
        {"six", 6},
        {"seven", 7},
        {"eight", 8},
        {"nine", 9}
      ] do
    def parse(<<unquote(string_number), rest::binary>>),
      do: [unquote(number) | parse(String.slice(unquote(string_number), 1..-1) <> rest)]
  end

  def parse(<<_, rest::binary>>), do: parse(rest)
  def parse(<<>>), do: []

  def decode(list) do
    List.first(list) * 10 + List.last(list)
  end

  def exclude_non_numeric(input) do
    String.replace(input, ~r/\D/, "")
  end

  def part1(input) do
    String.split(input, "\n", trim: true)
    # To avoid ["one",...] being parsed as [1,...] (part 1 constraint)
    |> Enum.map(&exclude_non_numeric/1)
    |> Enum.map(&parse/1)
    |> Enum.map(&decode/1)
    |> Enum.sum()
  end

  def part2(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&parse/1)
    |> Enum.map(&decode/1)
    |> Enum.sum()
  end
end
