defmodule AOC2023.D06 do
  def solve() do
    IO.puts("Day 6")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d06.txt")
  end

  def parse1(input) do
    input
    |> String.split("\n", trim: true, parts: 2)
    |> Enum.map(&String.trim/1)
    |> Enum.map(fn line ->
      line
      |> String.split(":", trim: true, parts: 2)
      |> List.last()
      |> String.split(" ", trim: true)
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.zip()
  end

  def parse2(input) do
    [time, distance] =
      input
      |> String.split("\n", trim: true, parts: 2)
      |> Enum.map(&String.trim/1)
      |> Enum.map(fn line ->
        line
        |> String.split(":", trim: true, parts: 2)
        |> List.last()
        |> String.split(" ", trim: true)
        |> Enum.join("")
        |> String.to_integer()
      end)

    {time, distance}
  end

  def solve(a, b, c) do
    delta = b * b - 4 * a * c

    if delta < 0 do
      {:error, "No real roots"}
    else
      x1 = (-b + delta ** 0.5) / (2 * a)
      x2 = (-b - delta ** 0.5) / (2 * a)
      {:ok, {x1, x2}}
    end
  end

  def wiggle_room({time, distance}) do
    case solve(-1, time, -(distance + 1)) do
      {:ok, {x1, x2}} -> floor(x2) - ceil(x1) + 1
      {:error, _} -> 0
    end
  end

  def part1(input) do
    input
    |> parse1()
    |> Enum.map(&wiggle_room/1)
    |> Enum.product()
  end

  def part2(input) do
    input
    |> parse2()
    |> wiggle_room()
  end
end
