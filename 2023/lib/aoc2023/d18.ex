defmodule AOC2023.D18 do
  @moduledoc """
  Documentation for `AOC2023.D18`.
  """

  def solve() do
    IO.puts("Day 18")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d18.txt")
  end

  def part1(input) do
    input
    |> parse_1()
    |> polygon_area()
  end

  def to_svg(vertices) do
    {{min_x, min_y}, {max_x, max_y}} =
      Enum.reduce(vertices, {{0, 0}, {0, 0}}, fn {x, y}, {{min_x, min_y}, {max_x, max_y}} ->
        {
          {min(min_x, x), min(min_y, y)},
          {max(max_x, x), max(max_y, y)}
        }
      end)

    {width, height} =
      {max_x - min_x, max_y - min_y}

    points =
      vertices
      |> Enum.map(fn {x, y} ->
        "#{Integer.to_string(x)},#{Integer.to_string(y)}"
      end)
      |> Enum.join(" ")

    """
    <svg width="512" height="512" viewBox="#{min_x} #{min_y} #{width} #{height}" xmlns="http://www.w3.org/2000/svg">
      <polygon points="#{points}" style="fill:lime;stroke:purple;stroke-width:1"/>
    </svg>
    """
  end

  def part2(input) do
    input
    |> parse_2()
    |> polygon_area()
  end

  def polygon_area(vertices) do
    vertices = vertices ++ Enum.take(vertices, 2)

    area =
      vertices
      |> Enum.chunk_every(3, 1, :discard)
      |> Enum.reduce(0, fn [{_, y0}, {x1, _}, {_, y2}], area ->
        area + x1 * (y2 - y0)
      end)

    area = area / 2
    trunc(area)
  end

  def vertexize(instructions) do
    {vertices, _} =
      instructions
      |> Enum.chunk_every(2, 1, [List.first(instructions)])
      |> Enum.map(fn [{a_dir, a_length}, {b_dir, _}] ->
        {a_dir, a_length, offset(a_dir, b_dir)}
      end)
      |> Enum.reduce({[], {0, 0}}, fn {dir, length, offset}, {vertices, coords} ->
        next = to_vector(dir, length) |> add(coords)
        vertex = next |> add(offset)
        {vertices ++ [vertex], next}
      end)

    vertices
  end

  def parse_1(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line_1/1)
    |> vertexize()
  end

  def parse_2(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line_2/1)
    |> vertexize()
  end

  def parse_line_1(line) do
    line
    |> String.split(" ", trim: true, parts: 3)
    |> then(fn [dir, length, _] ->
      {dir, String.to_integer(length)}
    end)
  end

  def parse_line_2(line) do
    line
    |> String.split(" ", trim: true, parts: 3)
    |> then(fn [_, _, color] ->
      color_to_dir_length(color)
    end)
  end

  def offset(dir_a, dir_b) do
    case {dir_a, dir_b} do
      {"R", "U"} -> {0, 0}
      {"R", "D"} -> {1, 0}
      {"U", "L"} -> {0, 1}
      {"U", "R"} -> {0, 0}
      {"L", "D"} -> {1, 1}
      {"L", "U"} -> {0, 1}
      {"D", "R"} -> {1, 0}
      {"D", "L"} -> {1, 1}
    end
  end

  @directions %{
    "U" => {0, -1},
    "L" => {-1, 0},
    "D" => {0, 1},
    "R" => {1, 0}
  }

  def color_to_dir_length(<<"(", "#", length::binary-size(5), dir::binary-size(1), ")">>) do
    {length, _} = Integer.parse(length, 16)

    {
      case dir do
        "0" -> "R"
        "1" -> "D"
        "2" -> "L"
        "3" -> "U"
      end,
      length
    }
  end

  def to_vector(dir, length) do
    {dx, dy} = @directions[dir]
    {dx * length, dy * length}
  end

  def add({x1, y1}, {x2, y2}) do
    {x1 + x2, y1 + y2}
  end
end
