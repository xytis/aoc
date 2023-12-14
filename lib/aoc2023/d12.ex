defmodule AOC2023.D12 do
  @moduledoc """
  Documentation for `AOC2023.D12`.
  """

  def solve() do
    IO.puts("Day 12")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d12.txt")
  end

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [springs, pools] = String.split(line, " ", trim: true, parts: 2)

      springs =
        String.split(springs, "", trim: true)
        |> Enum.map(fn spring ->
          case spring do
            "." -> :operational
            "#" -> :damaged
            "?" -> :unknown
          end
        end)

      pools =
        String.split(pools, ",", trim: true)
        |> Enum.map(&String.to_integer/1)

      {springs, pools}
    end)
  end

  def permutations(pools, length) do
    # pool, 0, pool, 0, pool
  end

  def is_valid_permutation?(permutation, spring) do
    Enum.zip(permutation, spring)
    |> Enum.all?(fn {p, s} ->
      case s do
        :unknown -> true
        :damaged -> p == :damaged
        :operational -> p == :operational
      end
    end)
  end

  def part1(input) do
    1
  end

  def part2(input) do
    2
  end
end
