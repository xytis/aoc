defmodule AOC2023.D07 do
  @order_part1 %{
    :_2 => 2,
    :_3 => 3,
    :_4 => 4,
    :_5 => 5,
    :_6 => 6,
    :_7 => 7,
    :_8 => 8,
    :_9 => 9,
    :_T => 10,
    :_J => 11,
    :_Q => 12,
    :_K => 13,
    :_A => 14,
    :high_card => 100,
    :one_pair => 200,
    :two_pairs => 300,
    :three_of_a_kind => 400,
    :full_house => 500,
    :four_of_a_kind => 600,
    :five_of_a_kind => 700
  }

  @order_part2 %{
    :_J => 1,
    :_2 => 2,
    :_3 => 3,
    :_4 => 4,
    :_5 => 5,
    :_6 => 6,
    :_7 => 7,
    :_8 => 8,
    :_9 => 9,
    :_T => 10,
    :_Q => 12,
    :_K => 13,
    :_A => 14,
    :high_card => 100,
    :one_pair => 200,
    :two_pairs => 300,
    :three_of_a_kind => 400,
    :full_house => 500,
    :four_of_a_kind => 600,
    :five_of_a_kind => 700
  }

  def solve() do
    IO.puts("Day 7")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d07.txt")
  end

  def parse_part1(line) do
    [cards, bid] = String.split(line, " ", trim: true, parts: 2)

    cards =
      cards
      |> String.split("", trim: true)
      |> Enum.map(fn card -> String.to_existing_atom("_" <> card) end)

    {[find_pattern_part1(cards) | cards], String.to_integer(bid)}
  end

  def find_pattern_part1(cards) do
    cards
    |> Enum.group_by(fn card -> @order_part1[card] end)
    |> Enum.map(fn {_, cards} -> Enum.count(cards) end)
    |> Enum.sort(:desc)
    |> case do
      [1 | _] -> :high_card
      [2, 1, 1, 1] -> :one_pair
      [2, 2, 1] -> :two_pairs
      [3, 1, 1] -> :three_of_a_kind
      [3, 2] -> :full_house
      [4, 1] -> :four_of_a_kind
      [5] -> :five_of_a_kind
    end
  end

  def order_part1(a, b) do
    a = @order_part1[a]
    b = @order_part1[b]

    cond do
      a < b -> :lt
      a > b -> :gt
      true -> :eq
    end
  end

  def compare_part1([], []), do: false

  def compare_part1([card1 | cards1], [card2 | cards2]) do
    case order_part1(card1, card2) do
      :lt -> true
      :gt -> false
      :eq -> compare_part1(cards1, cards2)
    end
  end

  def part1(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_part1/1)
    |> Enum.sort(fn {hand1, _}, {hand2, _} -> compare_part1(hand1, hand2) end)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {{_, bid}, index}, acc -> acc + index * bid end)
  end

  def parse_part2(line) do
    [cards, bid] = String.split(line, " ", trim: true, parts: 2)

    cards =
      cards
      |> String.split("", trim: true)
      |> Enum.map(fn card -> String.to_existing_atom("_" <> card) end)

    {[find_pattern_part2(cards) | cards], String.to_integer(bid)}
  end

  def apply_jokers({0, cards}), do: cards
  def apply_jokers({jokers, [head | cards]}), do: apply_jokers({jokers - 1, [head + 1 | cards]})
  def apply_jokers({5, []}), do: [5]

  def extract_jokers(cards) do
    {
      Enum.count(Map.get(cards, 1, [])),
      Map.drop(cards, [1])
      |> Enum.map(fn {_, cards} -> Enum.count(cards) end)
      |> Enum.sort(:desc)
    }
  end

  def find_pattern_part2(cards) do
    cards
    |> Enum.group_by(fn card -> @order_part2[card] end)
    |> extract_jokers()
    |> apply_jokers()
    |> case do
      [5] -> :five_of_a_kind
      [4, 1] -> :four_of_a_kind
      [3, 2] -> :full_house
      [3 | _] -> :three_of_a_kind
      [2 | [2 | _]] -> :two_pairs
      [2 | _] -> :one_pair
      [1 | _] -> :high_card
    end
  end

  def order_part2(a, b) do
    a = @order_part2[a]
    b = @order_part2[b]

    cond do
      a < b -> :lt
      a > b -> :gt
      true -> :eq
    end
  end

  def compare_part2([], []), do: false

  def compare_part2([card1 | cards1], [card2 | cards2]) do
    case order_part2(card1, card2) do
      :lt -> true
      :gt -> false
      :eq -> compare_part2(cards1, cards2)
    end
  end

  def part2(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_part2/1)
    |> Enum.sort(fn {hand1, _}, {hand2, _} -> compare_part2(hand1, hand2) end)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {{_, bid}, index}, acc -> acc + index * bid end)
  end
end
