defmodule AOC2023.D03 do
  def solve() do
    IO.puts("Day 3")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d03.txt")
  end

  def scan(line), do: scan(line, 0, :nothing)

  def scan(line, index, state)

  # def scan(<<"1", rest::binary>>, :nothing) do: scan(rest, {:number, 1})
  for integer <- ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] do
    def scan(<<unquote(integer), rest::binary>>, index, :nothing) do
      scan(rest, index + 1, {:number, String.to_integer(unquote(integer)), index})
    end
  end

  # def scan(<<"1", rest::binary>>, {:number, number}) do: scan(rest, {:number, number*10 + 1})
  for integer <- ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] do
    def scan(<<unquote(integer), rest::binary>>, index, {:number, number, start}) do
      scan(rest, index + 1, {:number, number * 10 + String.to_integer(unquote(integer)), start})
    end
  end

  # End number chains
  def scan(rest, index, {:number, number, start}) do
    [{:number, number, start, index - 1} | scan(rest, index, :nothing)]
  end

  # ignore '.'
  def scan(<<".", rest::binary>>, index, :nothing) do
    scan(rest, index + 1, :nothing)
  end

  # Symbol
  def scan(<<symbol, rest::binary>>, index, :nothing) do
    [{:symbol, <<symbol>>, index, index} | scan(rest, index + 1, :nothing)]
  end

  # End all chains
  def scan(<<>>, _, _), do: []

  def bounding_iterator(%Range{} = x_range, %Range{} = y_range) do
    # Compose a list of all coordinates in the bounding box
    List.flatten([
      # Top left
      [{x_range.first - 1, y_range.first - 1}],
      # Top
      Enum.map(x_range, fn x -> {x, y_range.first - 1} end),
      # Top right
      [{x_range.last + 1, y_range.first - 1}],
      # Right
      Enum.map(y_range, fn y -> {x_range.last + 1, y} end),
      # Bottom right
      [{x_range.last + 1, y_range.last + 1}],
      # Bottom
      Enum.map(x_range, fn x -> {x, y_range.last + 1} end),
      # Bottom left
      [{x_range.first - 1, y_range.last + 1}],
      # Left
      Enum.map(y_range, fn y -> {x_range.first - 1, y} end)
    ])
  end

  @doc """
  Parse square grid into a list of {:number, value, x_range, y_range} | {:symbol, value, x_range, y_range} tuples and a
  map of coordinates to {:number, value} | {:symbol, value} tuples.
  """
  @spec parse(binary()) :: {list(), map()}
  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&scan/1)
    |> Enum.with_index()
    |> Enum.map(fn {list, index} ->
      Enum.map(list, fn {type, value, start, stop} ->
        {type, value, start, stop, index}
      end)
    end)
    |> List.flatten()
    |> Enum.reduce({[], Map.new()}, fn {type, value, start, stop, y}, {list, map} ->
      {
        [{type, value, start..stop, y..y} | list],
        Enum.reduce(start..stop, map, fn x, acc ->
          Map.put(acc, {x, y}, {type, value})
        end)
      }
    end)
  end

  def part1(input) do
    {list, map} = parse(input)
    # Extract value those numbers that have a symbol in their bounding box, do 0 for others.
    Enum.map(list, fn
      {:number, value, x_range, y_range} ->
        case Enum.reduce(bounding_iterator(x_range, y_range), false, fn {x, y}, acc ->
               case Map.get(map, {x, y}) do
                 {:symbol, _string} -> true
                 _ -> acc
               end
             end) do
          true -> value
          false -> 0
        end

      _ ->
        0
    end)
    |> Enum.sum()
  end

  def part2(input) do
    {list, map} = parse(input)
    # For each gear (*) collect unique part numbers in their bounding box
    # If there are two, return gear ratio, else 0
    Enum.map(list, fn
      {:symbol, "*", x_range, y_range} ->
        parts =
          Enum.reduce(bounding_iterator(x_range, y_range), %{}, fn {x, y}, acc ->
            case Map.get(map, {x, y}) do
              {:number, value} -> Map.put(acc, value, true)
              _ -> acc
            end
          end)

        case map_size(parts) do
          2 -> Map.keys(parts) |> Enum.product()
          _ -> 0
        end

      _ ->
        0
    end)
    |> Enum.sum()
  end
end
