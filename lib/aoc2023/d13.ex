defmodule AOC2023.D13 do
  @moduledoc """
  Documentation for `AOC2023.D13`.
  """

  def solve() do
    IO.puts("Day 13")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d13.txt")
  end

  def parse(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(&read_and_flip/1)
  end

  def read_and_flip(input) do
    direct =
      input
      |> String.trim()
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split(&1, "", trim: true))

    inverted = transpose(direct)

    {
      direct,
      inverted
    }
  end

  def transpose(rows) do
    rows
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def count_differences(left, right) do
    Enum.zip(left, right)
    |> Enum.reduce(0, fn {l, r}, acc ->
      if l == r do
        acc
      else
        acc + 1
      end
    end)
  end

  def count_reflection_imperfections(inverse_left, right)

  def count_reflection_imperfections([left | left_rest], [right | right_rest]) do
    count_differences(left, right) + count_reflection_imperfections(left_rest, right_rest)
  end

  def count_reflection_imperfections([], []), do: 0
  def count_reflection_imperfections([], right) when is_list(right), do: 0
  def count_reflection_imperfections(left, []) when is_list(left), do: 0

  def scan_mirror(list, tolerance \\ 0, left \\ [])

  def scan_mirror([current | right], tolerance, left) do
    left = [current | left]

    cond do
      right == [] -> {:error, "no mirror found"}
      count_reflection_imperfections(left, right) == tolerance -> {:ok, Enum.count(left)}
      true -> scan_mirror(right, tolerance, left)
    end
  end

  def scan_mirror([], _tolerance, _left) do
    {:error, "no mirror found"}
  end

  def part1(input) do
    input
    |> parse()
    |> Enum.map(fn {direct, inverted} ->
      [
        case scan_mirror(direct) do
          {:ok, at} -> 100 * at
          {:error, _} -> 0
        end,
        case scan_mirror(inverted) do
          {:ok, at} -> at
          {:error, _} -> 0
        end
      ]
    end)
    |> List.flatten()
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> parse()
    |> Enum.map(fn {direct, inverted} ->
      [
        case scan_mirror(direct, 1) do
          {:ok, at} -> 100 * at
          {:error, _} -> 0
        end,
        case scan_mirror(inverted, 1) do
          {:ok, at} -> at
          {:error, _} -> 0
        end
      ]
    end)
    |> List.flatten()
    |> Enum.sum()
  end
end
