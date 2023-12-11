defmodule AOC2023.D11 do
  @moduledoc """
  Documentation for `AOC2023.D11`.
  """

  def solve() do
    IO.puts("Day 11")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d11.txt")
  end

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {line, row} ->
      line
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {tile, column} ->
        case tile do
          "." -> nil
          "#" -> {column, row}
        end
      end)
      |> Enum.filter(&(&1 != nil))
    end)
    |> List.flatten()
  end

  def find_empty_space(galaxies) do
    {rows, columns} =
      Enum.reduce(galaxies, {MapSet.new(), MapSet.new()}, fn {x, y}, {rows, columns} ->
        {MapSet.put(rows, y), MapSet.put(columns, x)}
      end)

    all_rows = MapSet.new(0..Enum.max(rows))
    all_columns = MapSet.new(0..Enum.max(columns))

    {MapSet.difference(all_rows, rows), MapSet.difference(all_columns, columns)}
  end

  def expand_space(galaxies, {rows, columns}) do
    galaxies
    |> Enum.map(fn {x, y} ->
      offset_x = Enum.filter(columns, fn column -> column < x end) |> Enum.count()
      offset_y = Enum.filter(rows, fn row -> row < y end) |> Enum.count()
      {x + offset_x, y + offset_y}
    end)
  end

  def mul(a, b) do
    a * b
  end

  def expand_ancient_space(galaxies, {rows, columns}) do
    galaxies
    |> Enum.map(fn {x, y} ->
      offset_x =
        Enum.filter(columns, fn column -> column < x end) |> Enum.count() |> mul(999_999)

      offset_y = Enum.filter(rows, fn row -> row < y end) |> Enum.count() |> mul(999_999)
      {x + offset_x, y + offset_y}
    end)
  end

  def distance({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  def cartesian(a, b) do
    for x <- a, y <- b, do: {x, y}
  end

  def part1(input) do
    galaxies = parse(input)

    empty = find_empty_space(galaxies)

    galaxies = expand_space(galaxies, empty)

    cartesian(galaxies, galaxies)
    |> Enum.map(fn {a, b} -> distance(a, b) end)
    |> Enum.sum()
    |> div(2)
  end

  def part2(input) do
    galaxies = parse(input)

    empty = find_empty_space(galaxies)

    galaxies = expand_ancient_space(galaxies, empty)

    cartesian(galaxies, galaxies)
    |> Enum.map(fn {a, b} -> distance(a, b) end)
    |> Enum.sum()
    |> div(2)
  end
end
