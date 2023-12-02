defmodule AOC2023.D02 do

  def solve() do
    IO.puts("Day 2")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d02.txt")
  end

  @doc """
  Parse a line of input into a tuple of game number and a list of plays.

  ## Examples

      iex> AOC2023.D02.parse("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
      {1, [[blue: 3, red: 4], [red: 1, green: 2, blue: 6], [green: 2]]}
  """
  def parse(line) do
    [game, plays] = String.split(line, ":", trim: true, parts: 2)
    [_, number] = String.split(game, " ", trim: true, parts: 2)
    number = String.to_integer(number)
    plays = String.split(plays, ";", trim: true)
    |> Enum.map(fn play ->
      String.split(play, ",", trim: true)
      |> Enum.map(fn draw ->
        [amount, color] = String.split(draw, " ", trim: true, parts: 2)
        {String.to_existing_atom(color), String.to_integer(amount)}
      end)
    end)
    {number, plays}
  end


  def part1(input) do
    totals = [red: 12, green: 13, blue: 14]

    String.split(input, "\n", trim: true)
    |> Enum.map(&parse/1)
    |> Enum.map(fn {index, plays} ->
       {index,
       Enum.map(plays, fn play ->
        Enum.reduce(play, true, fn {color, amount}, acc -> acc and (totals[color] >= amount) end)
       end)
       |> Enum.reduce(true, fn play, acc -> acc and play end)
      }
    end)
    |> Enum.filter(fn {_, valid} -> valid end)
    |> Enum.map(fn {index, _} -> index end)
    |> Enum.sum()
  end

  def part2(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&parse/1)
    |> Enum.map(fn {_, plays} ->
      Enum.reduce(plays, %{red: 0, green: 0, blue: 0}, fn play, acc ->
        Enum.reduce(play, acc, fn {color, amount}, acc ->
          Map.update(acc, color, amount, fn current -> max(current, amount) end)
        end)
      end)
      |> Enum.reduce(1, fn {_, v}, acc -> acc * v end)
    end)
    |> Enum.sum()
  end
end
