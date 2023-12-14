defmodule AOC2023.D14 do
  @moduledoc """
  Documentation for `AOC2023.D14`.
  """

  def solve() do
    IO.puts("Day 14")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d14.txt")
  end

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, "", trim: true))
  end

  def transpose(matrix) do
    matrix
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def rotate_anticlockwise(matrix) do
    matrix
    |> transpose()
    |> Enum.reverse()
  end

  def rotate_clockwise(matrix) do
    matrix
    |> transpose()
    |> Enum.map(&Enum.reverse/1)
  end

  def tilt_left(rows) do
    rows
    |> Enum.map(&improved_roll/1)
  end

  def count_load(row) do
    row
    |> Enum.reverse()
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {value, index}, acc ->
      case value do
        "O" -> acc + index
        _ -> acc
      end
    end)
  end

  def count_total_load(matrix) do
    matrix
    |> Enum.map(&count_load/1)
    |> Enum.sum()
  end

  def do_improved_roll({0, 0}, []), do: []
  def do_improved_roll({0, spaces}, []), do: ["." | do_improved_roll({0, spaces - 1}, [])]

  def do_improved_roll({stones, spaces}, []),
    do: ["O" | do_improved_roll({stones - 1, spaces - 1}, [])]

  def do_improved_roll({0, 0}, ["#" | rest]), do: ["#" | do_improved_roll({0, 0}, rest)]

  def do_improved_roll({0, spaces}, ["#" | rest]),
    do: ["." | do_improved_roll({0, spaces - 1}, ["#" | rest])]

  def do_improved_roll({stones, spaces}, ["#" | rest]),
    do: ["O" | do_improved_roll({stones - 1, spaces - 1}, ["#" | rest])]

  def do_improved_roll({stones, spaces}, ["." | rest]),
    do: do_improved_roll({stones, spaces + 1}, rest)

  def do_improved_roll({stones, spaces}, ["O" | rest]),
    do: do_improved_roll({stones + 1, spaces + 1}, rest)

  def do_roll([]), do: []

  def do_roll([x]), do: [x]

  def do_roll([this | that = [next | rest]]) do
    case {this, next} do
      {_, "#"} -> Enum.concat([this, next], do_roll(rest))
      {".", "O"} -> Enum.concat([next], do_roll([this | rest]))
      _ -> Enum.concat([this], do_roll(that))
    end
  end

  def improved_roll(stones) do
    do_improved_roll({0, 0}, stones)
  end

  def roll(stones) do
    rolled =
      do_improved_roll({0, 0}, stones)

    # If no change, terminate
    case rolled == stones do
      true -> rolled
      false -> roll(rolled)
    end
  end

  def cycle(matrix) do
    matrix
    # North
    |> tilt_left()
    |> rotate_clockwise()
    # West
    |> tilt_left()
    |> rotate_clockwise()
    # South
    |> tilt_left()
    |> rotate_clockwise()
    # East
    |> tilt_left()
    |> rotate_clockwise()
  end

  def part1(input) do
    input
    |> parse()
    |> rotate_anticlockwise()
    |> tilt_left()
    |> count_total_load()
  end

  def find_cycle(map, matrix, index \\ 0) do
    # IO.inspect(map)
    next = AOC2023.D14.cycle(matrix)
    compact = List.flatten(next) |> Enum.join()

    case Map.get(map, compact) do
      nil -> find_cycle(Map.put(map, compact, index), next, index + 1)
      at -> {matrix, at..(index - 1)}
    end
  end

  def part2(input) do
    matrix =
      input
      |> parse()
      |> rotate_anticlockwise()

    {matrix, from..to} = find_cycle(%{}, matrix)

    # IO.inspect({matrix, from..to})

    offset =
      (1_000_000_000 - from)
      |> rem(to - from + 1)

    # IO.inspect({offset, matrix})

    Stream.unfold({offset, matrix}, fn {offset, matrix} ->
      if offset > 0 do
        matrix =
          AOC2023.D14.cycle(matrix)

        {matrix, {offset - 1, matrix}}
      else
        nil
      end
    end)
    |> Enum.to_list()
    # |> IO.inspect()
    |> List.last()
    |> count_total_load()
  end
end
