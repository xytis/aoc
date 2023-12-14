defmodule AOC2023.D12 do
  @moduledoc """
  Documentation for `AOC2023.D12`.
  """

  def solve() do
    IO.puts("Day 12")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d12.txt")
  end

  def parse(input, amount \\ 1) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line(&1, amount))
  end

  def parse_line(line, amount) do
    line
    |> String.split(" ", trim: true, parts: 2)
    |> unfold(amount)
    |> parse_springs_pools()
  end

  def parse_springs_pools([springs, pools]) do
    springs =
      String.split(springs, "", trim: true)

    pools =
      String.split(pools, ",", trim: true)
      |> Enum.map(&String.to_integer/1)

    {springs, pools}
  end

  def unfold([springs, pools], amount) do
    springs = for _ <- 1..amount, do: springs
    pools = for _ <- 1..amount, do: pools

    [springs |> Enum.join("?"), pools |> Enum.join(",")]
  end

  def is_valid?(springs, pools)

  def is_valid?([], []), do: true
  def is_valid?([], [{0}]), do: true
  def is_valid?([], _), do: false

  def is_valid?(["#" | _], [{0} | _]), do: false

  # A failing pool starts with a untouched pool
  def is_valid?(["#" | remaining_springs], [current | remaining_pools]) when is_number(current),
    do: is_valid?(remaining_springs, [{current - 1} | remaining_pools])

  def is_valid?(["#" | remaining_springs], [{current} | remaining_pools]),
    do: is_valid?(remaining_springs, [{current - 1} | remaining_pools])

  # Each failing pool must terminate with a spring
  def is_valid?(["." | remaining_springs], [{0} | remaining_pools]),
    do: is_valid?(remaining_springs, remaining_pools)

  # You can not terminate non-completed pools with a spring
  def is_valid?(["." | _], [{_} | _]), do: false

  def is_valid?(["." | remaining_springs], pools), do: is_valid?(remaining_springs, pools)

  def is_valid?(_, []), do: false

  def count_valid(known, springs, pools)

  def count_valid(known, [], []), do: {known, 1}
  def count_valid(known, [], [{0}]), do: {known, 1}
  def count_valid(known, [], _), do: {known, 0}

  def count_valid(known, ["#" | _], [{0} | _]), do: {known, 0}

  def count_valid(known, ["#" | remaining_springs], [current | remaining_pools])
      when is_number(current),
      do: count_valid(known, remaining_springs, [{current - 1} | remaining_pools])

  def count_valid(known, ["#" | remaining_springs], [{current} | remaining_pools]),
    do: count_valid(known, remaining_springs, [{current - 1} | remaining_pools])

  def count_valid(known, ["." | remaining_springs], [{0} | remaining_pools]),
    do: count_valid(known, remaining_springs, remaining_pools)

  def count_valid(known, ["." | _], [{_} | _]), do: {known, 0}

  def count_valid(known, ["." | remaining_springs], pools),
    do: count_valid(known, remaining_springs, pools)

  def count_valid(known, ["?" | remaining_springs], pools) do
    {known, left} =
      case Map.get(known, {["#" | remaining_springs], pools}) do
        nil -> count_valid(known, ["#" | remaining_springs], pools)
        c -> {known, c}
      end

    known = Map.put(known, {["#" | remaining_springs], pools}, left)

    {known, right} =
      case Map.get(known, {["." | remaining_springs], pools}) do
        nil -> count_valid(known, ["." | remaining_springs], pools)
        c -> {known, c}
      end

    known = Map.put(known, {["." | remaining_springs], pools}, right)

    {known, left + right}
  end

  def count_valid(known, _, []), do: {known, 0}

  def part1(input) do
    input
    |> parse()
    |> Enum.map(fn {springs, pools} ->
      {_, count} = count_valid(%{}, springs, pools)
      count
    end)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> parse(5)
    |> Enum.map(fn {springs, pools} ->
      {_, count} = count_valid(%{}, springs, pools)
      count
    end)
    |> Enum.sum()
  end
end
