defmodule AOC2023.D10 do
  @moduledoc """
  Documentation for `AOC2023.D10`.
  """

  @directions %{
    "N" => {0, -1},
    "E" => {1, 0},
    "S" => {0, 1},
    "W" => {-1, 0}
  }

  @exit_to_entry %{
    "N" => "S",
    "E" => "W",
    "S" => "N",
    "W" => "E"
  }

  @tiles %{
    "|" => {"N", "S"},
    "-" => {"E", "W"},
    "L" => {"E", "N"},
    "J" => {"N", "W"},
    "7" => {"S", "W"},
    "F" => {"E", "S"},
    "S" => {:start},
    "." => {:ground}
  }

  @definitions %{
    {"N", "S"} => {"I", "O", "I", "O"},
    {"E", "W"} => {"I", "I", "O", "O"},
    {"E", "N"} => {"I", "O", "I", "I"},
    {"N", "W"} => {"I", "O", "O", "O"},
    {"S", "W"} => {"O", "O", "I", "O"},
    {"E", "S"} => {"O", "O", "O", "I"},
    {:ground} => {"O", "O", "O", "O"}
  }

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {line, y} ->
      String.split(line, "", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {tile, x} ->
        case @tiles[tile] do
          {:start} -> {:start, {x, y}}
          tile -> {{x, y}, tile}
        end
      end)
    end)
    |> List.flatten()
    |> Map.new()
  end

  def solve() do
    IO.puts("Day 10")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d10.txt")
  end

  def step({x, y}, direction) do
    {dx, dy} = @directions[direction]
    {x + dx, y + dy}
  end

  def walk(map, [{{x, y}, exit_direction, path} | rest]) do
    # IO.inspect({{x, y}, direction, path}, label: "Walking")
    next = step({x, y}, exit_direction)
    # IO.inspect(next, label: "Next")
    entry_direction = @exit_to_entry[exit_direction]
    # IO.inspect(entry, label: "Entry")

    case (case Map.get(map, next) do
            nil ->
              []

            {:ground} ->
              []

            {:start} ->
              {:done}

            {^entry_direction, exit} ->
              [{next, exit, [{x, y} | path]}]

            {exit, ^entry_direction} ->
              [{next, exit, [{x, y} | path]}]

            # Unconnected tile
            {_, _} ->
              []
          end) do
      {:done} -> [{x, y} | path] |> Enum.reverse()
      [] -> walk(map, rest)
      some -> walk(map, rest ++ some)
    end
  end

  def walk(_map, []) do
    []
  end

  def extract_start(map) do
    {start, map} = Map.pop(map, :start)
    map = Map.put(map, start, {:start})

    {start, map}
  end

  def cmp(a, b) do
    cond do
      a < b -> :lt
      a > b -> :gt
      true -> :eq
    end
  end

  def scout(map) do
    {from, map} = extract_start(map)

    [{a, da} | [{_, db} | _]] =
      [
        {walk(map, [{from, "N", []}]), "N"},
        {walk(map, [{from, "E", []}]), "E"},
        {walk(map, [{from, "S", []}]), "S"},
        {walk(map, [{from, "W", []}]), "W"}
      ]
      |> Enum.sort(fn {a, da}, {b, db} ->
        # To ensure that start tile is among defined tiles, we sort directions alphabetically
        case cmp(Enum.count(a), Enum.count(b)) do
          :eq -> da < db
          :gt -> true
          :lt -> false
        end
      end)

    map = Map.put(map, from, {da, db})
    {map, a}
  end

  def clean_map({map, path}) do
    filter = Map.new(path, fn coords -> {coords, true} end)

    map
    |> Enum.map(fn {coords, tile} ->
      case Map.get(filter, coords) do
        nil -> {coords, {:ground}}
        true -> {coords, tile}
      end
    end)
    |> Map.new()
  end

  def flip(definition_from, definition_to, value) do
    cond do
      definition_from == definition_to ->
        value

      definition_from != definition_to ->
        case value do
          "I" -> "O"
          "O" -> "I"
        end
    end
  end

  def side(direction, {d0, d1, d2, d3}) do
    case direction do
      "N" -> {d0, d1}
      "W" -> {d0, d2}
      "S" -> {d2, d3}
      "E" -> {d1, d3}
    end
  end

  def entry_to_exits(entry) do
    case entry do
      "N" -> ["E", "S", "W"]
      "W" -> ["N", "E", "S"]
      "S" -> ["W", "N", "E"]
      "E" -> ["S", "W", "N"]
    end
  end

  def generate_exits(shape, {coords, {entry_direction, {v0, v1}}}) do
    definition = @definitions[shape]
    {ed0, ed1} = side(entry_direction, definition)

    entry_to_exits(entry_direction)
    |> Enum.map(fn exit_direction ->
      {
        step(coords, exit_direction),
        {@exit_to_entry[exit_direction],
         {
           flip(ed0, elem(side(exit_direction, definition), 0), v0),
           flip(ed1, elem(side(exit_direction, definition), 1), v1)
         }}
      }
    end)
  end

  def fill(map, visited, [{coords, {entry_direction, {l, r}}} | rest]) do
    with_nest =
      case {l, r} do
        {"I", _} -> 1
        {_, "I"} -> 1
        _ -> 0
      end

    # IO.inspect(visited, label: "Visited")
    # IO.inspect({coords, {entry_direction, {l, r}}}, label: "Current")

    case {Map.get(map, coords), Map.get(visited, coords)} do
      # Out of bounds
      {nil, nil} ->
        fill(map, visited, rest)

      # Not visited groud
      {{:ground}, nil} ->
        with_nest +
          fill(
            map,
            Map.put(visited, coords, with_nest),
            rest ++ generate_exits({:ground}, {coords, {entry_direction, {l, r}}})
          )

      # Not visited symbol
      {shape, nil} ->
        fill(
          map,
          Map.put(visited, coords, 0),
          rest ++ generate_exits(shape, {coords, {entry_direction, {l, r}}})
        )

      # Already visited
      {_, _} ->
        fill(map, visited, rest)
    end
  end

  def fill(_, _, []) do
    0
  end

  def part1(input) do
    input
    |> parse()
    |> scout()
    |> then(fn {_map, path} -> path end)
    |> Enum.count()
    |> then(fn x -> floor(x / 2) end)
  end

  def part2(input) do
    input
    |> parse()
    |> scout()
    |> clean_map()
    |> then(fn map -> fill(map, %{}, [{{0, 0}, {"N", {"O", "O"}}}]) end)
  end
end
