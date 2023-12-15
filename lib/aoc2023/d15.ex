defmodule AOC2023.D15 do
  @moduledoc """
  Documentation for `AOC2023.D15`.
  """

  def solve() do
    IO.puts("Day 15")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d15.txt")
  end

  def parse(input) do
    input
    |> String.replace("\n", "")
    |> String.split(",", trim: true)
  end

  def tokenify(instruction) do
    case Regex.named_captures(~r/(?<label>.+)(?<op>[=-])(?<focal>\d+)?/, instruction) do
      %{"label" => label, "op" => "=", "focal" => focal} ->
        {hash(label), :assign, String.to_atom(label), String.to_integer(focal)}

      %{"label" => label, "op" => "-"} ->
        {hash(label), :remove, String.to_atom(label)}
    end
  end

  def perform(operation, boxes) do
    case operation do
      {hash, :assign, label, focal} ->
        boxes = Map.put_new(boxes, hash, Keyword.new())

        Map.update!(boxes, hash, fn lenses ->
          if Keyword.has_key?(lenses, label) do
            Keyword.replace(lenses, label, focal)
          else
            lenses ++ [{label, focal}]
          end
        end)

      {hash, :remove, label} ->
        Map.update(boxes, hash, [], fn lenses ->
          Keyword.delete(lenses, label)
        end)
    end
  end

  def box_focusing_power(box_index, lenses) do
    lenses
    |> Enum.with_index(1)
    |> Enum.map(fn {{_, focal}, index} ->
      box_index * index * focal
    end)
    |> Enum.sum()
  end

  def hash(str, acc \\ 0)

  def hash(<<letter, rest::binary>>, acc) do
    acc = acc + letter
    acc = acc * 17
    acc = rem(acc, 256)
    hash(rest, acc)
  end

  def hash("", acc), do: acc

  def part1(input) do
    input
    |> parse()
    |> Enum.map(&hash(&1))
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> parse()
    |> Enum.map(&tokenify/1)
    |> Enum.reduce(%{}, &perform/2)
    |> Enum.map(fn {box_index, lenses} ->
      box_focusing_power(box_index + 1, lenses)
    end)
    |> Enum.sum()
  end
end
