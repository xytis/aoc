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

  def match(<<k1, k2, k3, " = (", l1, l2, l3, ", ", r1, r2, r3, ")">>),
    do: {<<k1, k2, k3>>, {<<l1, l2, l3>>, <<r1, r2, r3>>}}

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

  def walk_part1({instructions, map}), do: walk_part1({instructions, map}, instructions, "AAA", 0)

  def walk_part1({instructions, map}, [], current, step) do
    walk_part1({instructions, map}, instructions, current, step)
  end

  def walk_part1(_, _, "ZZZ", step), do: {:ok, step}

  def walk_part1({instructions, map}, [next | rest], current, step) do
    case {Map.get(map, current), next} do
      {nil, _} -> {:error, step}
      {{next, _}, "L"} -> walk_part1({instructions, map}, rest, next, step + 1)
      {{_, next}, "R"} -> walk_part1({instructions, map}, rest, next, step + 1)
    end
  end

  def step({instructions, map}, start),
    do: {:next, {{instructions, map}, instructions, start, 0}}

  def step({:next, {{instructions, map}, [next | rest], current, step}}) do
    case {Map.get(map, current), next} do
      {{next, _}, "L"} -> {:next, {{instructions, map}, rest, next, step + 1}}
      {{_, next}, "R"} -> {:next, {{instructions, map}, rest, next, step + 1}}
    end
  end

  def step({:next, {{instructions, map}, [], current, step}}) do
    step({:next, {{instructions, map}, instructions, current, step}})
  end

  def part1(input) do
    input
    |> parse()
    |> walk_part1()
    |> case do
      {:ok, step} -> step
      _ -> 0
    end
  end

  def loop({:next, {{instructions, map}, path, current, step}}) do
    case current do
      <<_, _, "Z">> ->
        step

      _ ->
        loop(step({:next, {{instructions, map}, path, current, step}}))
    end
  end

  def gcd(a, 0), do: a
  def gcd(0, b), do: b
  def gcd(a, b), do: gcd(b, rem(a, b))

  def lcm(0, 0), do: 0
  def lcm(a, b), do: floor(a * b / gcd(a, b))

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
    |> Enum.map(fn start -> step({instructions, map}, start) end)
    # Manual discovery was made: the input data ensures that each path is a loop that starts and ends at the same point.
    |> Enum.map(&loop/1)
    |> Enum.reduce(&lcm/2)

    # |> loop()
  end
end
