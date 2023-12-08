defmodule AOC2023.D08 do
  @moduledoc """
  Documentation for `AOC2023.D08`.
  """

  def solve() do
    IO.puts("Day 8")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d08.txt")
  end

  def match(<<k::binary-size(3), " = (", l::binary-size(3), ", ", r::binary-size(3), ")">>),
    do: {k, {l, r}}

  @spec parse(binary()) :: {[binary()], map()}
  def parse(input) do
    [pattern, map] = String.split(input, "\n\n", trim: true, parts: 2)

    {
      String.split(pattern, "", trim: true),
      map
      |> String.split("\n", trim: true)
      |> Enum.map(&match/1)
      |> Map.new()
    }
  end

  def step({{instructions, map}, [next | rest], current, step}) do
    case {Map.get(map, current), next} do
      {{next, _}, "L"} -> {{instructions, map}, rest, next, step + 1}
      {{_, next}, "R"} -> {{instructions, map}, rest, next, step + 1}
    end
  end

  def step({{instructions, map}, [], current, step}) do
    step({{instructions, map}, instructions, current, step})
  end

  def walk({_, _, current, iteration} = state) do
    case current do
      <<_, _, "Z">> -> iteration
      _ -> walk(step(state))
    end
  end

  def gcd(a, 0), do: a
  def gcd(0, b), do: b
  def gcd(a, b), do: gcd(b, rem(a, b))

  def lcm(0, 0), do: 0
  def lcm(a, b), do: floor(a * b / gcd(a, b))

  def part1(input) do
    {instructions, map} =
      input
      |> parse()

    walk({{instructions, map}, [], "AAA", 0})
  end

  def part2(input) do
    {instructions, map} =
      input
      |> parse()

    # Find all starting positions:
    Map.keys(map)
    |> Enum.filter(fn
      <<_, _, "A">> -> true
      <<_, _, _>> -> false
    end)
    |> Enum.map(fn start -> {{instructions, map}, [], start, 0} end)
    # Manual discovery was made: the input data ensures that each path is a walk that starts
    # and ends at the same point and with the same state.
    |> Enum.map(&walk/1)
    # So we only need to find the least common multiple of all paths
    |> Enum.reduce(&lcm/2)
  end
end
