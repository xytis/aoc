defmodule AOC2023.D09 do
  @moduledoc """
  Documentation for `AOC2023.D09`.
  """

  def solve() do
    IO.puts("Day 9")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d09.txt")
  end

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      String.split(line, " ", trim: true)
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def resolve(list) do
    list
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [first, second] -> second - first end)
  end

  def all_zeroes?(list) do
    Enum.all?(list, fn x -> x == 0 end)
  end

  def resolve_recursively(list) do
    resolve_recursively(list, [list])
  end

  def resolve_recursively(list, history) do
    next = resolve(list)

    cond do
      all_zeroes?(next) -> history
      true -> resolve_recursively(next, [next | history])
    end
  end

  def sum_last(list) do
    Enum.reduce(list, 0, fn x, acc -> acc + List.last(x) end)
  end

  def substract_first(list) do
    Enum.reduce(list, 0, fn x, acc -> List.first(x) - acc end)
  end

  def part1(input) do
    input
    |> parse()
    |> Enum.map(&resolve_recursively/1)
    |> Enum.map(&sum_last/1)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> parse()
    |> Enum.map(&resolve_recursively/1)
    |> Enum.map(&substract_first/1)
    |> Enum.sum()
  end
end
