defmodule AOC2023.D16 do
  @moduledoc """
  Documentation for `AOC2023.D16`.
  """

  @directions %{
    "^" => {0, -1},
    "<" => {-1, 0},
    "v" => {0, 1},
    ">" => {1, 0}
  }

  def step({x, y}, direction) do
    {dx, dy} = @directions[direction]
    {x + dx, y + dy}
  end

  def reflect(direction, mirror) do
    case {direction, mirror} do
      {"^", "/"} -> ">"
      {"^", "\\"} -> "<"
      {"<", "/"} -> "v"
      {"<", "\\"} -> "^"
      {"v", "/"} -> "<"
      {"v", "\\"} -> ">"
      {">", "/"} -> "^"
      {">", "\\"} -> "v"
    end
  end

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {line, y} ->
      String.split(line, "", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {tile, x} ->
        {{x, y}, {tile, []}}
      end)
    end)
    |> List.flatten()
    |> Map.new()
  end

  def propagate(grid, {x, y}, direction) do
    case Map.get(grid, {x, y}) do
      nil ->
        grid

      {tile, beams} ->
        # exit on exiting beam
        if direction in beams do
          grid
        else
          # Register this beam
          grid = Map.put(grid, {x, y}, {tile, [direction | beams]})
          # Act on the tile
          case {direction, tile} do
            {direction, mirror} when mirror in ["\\", "/"] ->
              direction = reflect(direction, mirror)
              propagate(grid, step({x, y}, direction), direction)

            {direction, "-"} when direction in ["^", "v"] ->
              direction = "<"
              grid = propagate(grid, step({x, y}, direction), direction)
              direction = ">"
              grid = propagate(grid, step({x, y}, direction), direction)
              grid

            {direction, "|"} when direction in ["<", ">"] ->
              direction = "^"
              grid = propagate(grid, step({x, y}, direction), direction)
              direction = "v"
              grid = propagate(grid, step({x, y}, direction), direction)
              grid

            {direction, _} ->
              propagate(grid, step({x, y}, direction), direction)
          end
        end
    end
  end

  def count_energised(grid) do
    grid
    |> Map.values()
    |> Enum.filter(fn {_, beams} -> length(beams) > 0 end)
    |> Enum.count()
  end

  def solve() do
    IO.puts("Day 16")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d16.txt")
  end

  def part1(input) do
    input
    |> parse()
    |> propagate({0, 0}, ">")
    |> count_energised()
  end

  def part2(input) do
    grid = input |> parse()
    min = 0
    max = (:math.sqrt(map_size(grid)) |> trunc()) - 1

    for entry <- min..max do
      [
        {{min, entry}, propagate(grid, {min, entry}, ">") |> count_energised()},
        {{entry, min}, propagate(grid, {entry, min}, "v") |> count_energised()},
        {{max, entry}, propagate(grid, {max, entry}, "<") |> count_energised()},
        {{entry, max}, propagate(grid, {entry, max}, "^") |> count_energised()}
      ]
    end
    |> List.flatten()
    |> Enum.max_by(fn {_, count} -> count end)
    |> elem(1)
  end
end
