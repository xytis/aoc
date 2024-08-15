defmodule AOC2023.D04 do
  def solve() do
    IO.puts("Day 4")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d04.txt")
  end

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      [header, numbers] = String.split(line, ":", trim: true, parts: 2)
      [_name, index] = String.split(header, " ", trim: true, parts: 2)
      index = String.to_integer(String.trim(index))

      [
        index
        | String.split(numbers, "|", trim: true, parts: 2)
          |> Enum.map(fn numbers ->
            String.split(numbers, " ", trim: true)
            |> Enum.map(&String.to_integer/1)
          end)
      ]
    end)
  end

  def score([_index, winning_numbers, all_numbers]) do
    set = MapSet.new(winning_numbers)

    case Enum.reduce(all_numbers, 0, fn number, acc ->
           if Enum.member?(set, number) do
             acc + 1
           else
             acc
           end
         end) do
      0 -> 0
      power -> :math.pow(2, power - 1) |> round
    end
  end

  def victories([index, winning_numbers, all_numbers]) do
    set = MapSet.new(winning_numbers)

    {
      index,
      case Enum.reduce(all_numbers, 0, fn number, acc ->
             if Enum.member?(set, number) do
               acc + 1
             else
               acc
             end
           end) do
        0 -> []
        amount -> (index + 1)..(index + amount) |> Enum.to_list()
      end
    }
  end

  def count([card | rest], lookup) do
    case lookup[card] do
      list when is_list(list) ->
        score = count(list, lookup)
        Map.update(lookup, card, score, fn _ -> score end)
        1 + score

      score when is_integer(score) ->
        1 + score
    end + count(rest, lookup)
  end

  def count([], _lookup), do: 0

  def part1(input) do
    parse(input)
    |> Enum.map(&score/1)
    |> Enum.sum()
  end

  def part2(input) do
    {list, lookup} =
      parse(input)
      |> Enum.map(&victories/1)
      |> Enum.reduce({[], Map.new()}, fn {index, victories}, {cards, lookup} ->
        {[index | cards], Map.put(lookup, index, victories)}
      end)

    count(list, lookup)
  end
end
