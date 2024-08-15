defmodule AOC2023.D17 do
  @moduledoc """
  Documentation for `AOC2023.D17`.
  """

  def solve() do
    IO.puts("Day 17")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d17.txt")
  end

  def part1(input) do
    {map, max_x, max_y} =
      input
      |> parse()

    path =
      process(
        &next_simple_crucible/3,
        fn _ -> true end,
        map,
        [
          {{0, 1}, ["v"], {:start}, 0},
          {{1, 0}, [">"], {:start}, 0}
        ],
        %{},
        {max_x, max_y}
      )

    # IO.inspect(path, label: "Path")
    # print({map, max_x, max_y}, path)

    {_, _, cost} = List.last(path)
    cost
  end

  def part2(input) do
    {map, max_x, max_y} =
      input
      |> parse()

    path =
      process(
        &next_ultra_crucible/3,
        fn path -> length(path) >= 4 end,
        map,
        [
          {{0, 1}, ["v"], {:start}, 0},
          {{1, 0}, [">"], {:start}, 0}
        ],
        %{},
        {max_x, max_y}
      )

    # IO.inspect(path, label: "Path")
    # print({map, max_x, max_y}, path)

    {_, _, cost} = List.last(path)
    cost
  end

  def print({map, max_x, max_y}, path) do
    path = Enum.map(path, fn {{x, y}, l, _} -> {{x, y}, List.last(l)} end) |> Map.new()

    for y <- 0..max_y do
      for x <- 0..max_x do
        case Map.fetch(path, {x, y}) do
          {:ok, symbol} ->
            IO.write("#{symbol}")

          :error ->
            Map.fetch!(map, {x, y})
            |> Integer.to_string()
            |> IO.write()
        end
      end

      IO.write("\n")
    end
  end

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

  def left(dir) do
    case dir do
      "^" -> "<"
      "<" -> "v"
      "v" -> ">"
      ">" -> "^"
    end
  end

  def right(dir) do
    case dir do
      "^" -> ">"
      "<" -> "^"
      "v" -> "<"
      ">" -> "v"
    end
  end

  def next_simple_crucible(coords, path, cost) do
    case path do
      [dir] ->
        l = left(dir)
        r = right(dir)

        [
          {step(coords, l), [l], {coords, path}, cost},
          {step(coords, dir), [dir, dir], {coords, path}, cost},
          {step(coords, r), [r], {coords, path}, cost}
        ]

      [dir, dir] ->
        l = left(dir)
        r = right(dir)

        [
          {step(coords, l), [l], {coords, path}, cost},
          {step(coords, dir), [dir, dir, dir], {coords, path}, cost},
          {step(coords, r), [r], {coords, path}, cost}
        ]

      [dir, dir, dir] ->
        l = left(dir)
        r = right(dir)

        [
          {step(coords, l), [l], {coords, path}, cost},
          {step(coords, r), [r], {coords, path}, cost}
        ]
    end
  end

  def next_ultra_crucible(coords, path, cost) do
    case path do
      [dir] ->
        [{step(coords, dir), [dir, dir], {coords, path}, cost}]

      [dir, dir] ->
        [{step(coords, dir), [dir, dir, dir], {coords, path}, cost}]

      [dir, dir, dir] ->
        [{step(coords, dir), [dir, dir, dir, dir], {coords, path}, cost}]

      [dir, dir, dir, dir, dir, dir, dir, dir, dir, dir] ->
        l = left(dir)
        r = right(dir)

        [
          {step(coords, l), [l], {coords, path}, cost},
          {step(coords, r), [r], {coords, path}, cost}
        ]

      [dir | rest] ->
        l = left(dir)
        r = right(dir)

        [
          {step(coords, l), [l], {coords, path}, cost},
          {step(coords, dir), [dir | [dir | rest]], {coords, path}, cost},
          {step(coords, r), [r], {coords, path}, cost}
        ]
    end
  end

  def process(
        next,
        valid_stop,
        map,
        [{coords, path, prev, cost} | rest],
        visited,
        goal = {_max_x, _max_y}
      ) do
    case Map.fetch(map, coords) do
      :error ->
        # Ignore out of bounds
        process(next, valid_stop, map, rest, visited, goal)

      {:ok, value} ->
        cost = cost + value

        case Map.fetch(visited, {coords, path}) do
          {:ok, {_, previous_cost}} when cost <= previous_cost ->
            # Reconsider already visited node, if current entry is cheaper or same
            visited = Map.delete(visited, {coords, path})
            process(next, valid_stop, map, [{coords, path, prev, cost} | rest], visited, goal)

          {:ok, _} ->
            process(next, valid_stop, map, rest, visited, goal)

          :error ->
            visited = Map.put(visited, {coords, path}, {prev, cost})

            # IO.puts("\n")
            # print({map, max_x, max_y}, recover_path({coords, path}, visited))

            remaining =
              (rest ++ next.(coords, path, cost))
              |> Enum.sort_by(fn {_, _, _, cost} -> cost end)

            if coords == goal && valid_stop.(path) do
              recover_path({coords, path}, visited)
            else
              process(next, valid_stop, map, remaining, visited, goal)
            end
        end
    end
  end

  def recover_path({coords, path}, visited) do
    case Map.fetch(visited, {coords, path}) do
      {:ok, {prev, cost}} ->
        recover_path(prev, visited) ++ [{coords, path, cost}]

      :error ->
        []
    end
  end

  def recover_path({:start}, _), do: []

  def parse(input) do
    map =
      input
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {line, y} ->
        String.split(line, "", trim: true)
        |> Enum.with_index()
        |> Enum.map(fn {tile, x} ->
          {{x, y}, String.to_integer(tile)}
        end)
      end)
      |> List.flatten()
      |> Map.new()

    max_x = Enum.max(Enum.map(Map.keys(map), fn {x, _} -> x end))
    max_y = Enum.max(Enum.map(Map.keys(map), fn {_, y} -> y end))

    {map, max_x, max_y}
  end
end
