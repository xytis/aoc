defmodule AOC2023.D21 do
  @moduledoc """
  Documentation for `AOC2023.D21`.
  """

  defmodule Grid do
    defstruct [:grid, :size, :infinite]

    def parse(input) do
      grid =
        input
        |> String.split("\n", trim: true)
        |> Enum.with_index()
        |> Enum.map(fn {line, y} ->
          String.split(line, "", trim: true)
          |> Enum.with_index()
          |> Enum.map(fn {tile, x} ->
            case tile_to_symbol(tile) do
              :start -> [{{x, y}, :garden}, {:start, {x, y}}]
              symbol -> {{x, y}, symbol}
            end
          end)
          |> List.flatten()
        end)

      {start, grid} =
        grid
        |> List.flatten()
        |> Map.new()
        |> Map.pop(:start)

      {start,
       %__MODULE__{
         grid: grid,
         size: floor(:math.sqrt(map_size(grid))),
         infinite: false
       }}
    end

    def make_infinite(%__MODULE__{} = grid) do
      %__MODULE__{
        grid: grid.grid,
        size: grid.size,
        infinite: true
      }
    end

    defp tile_to_symbol(tile) do
      case tile do
        "." -> :garden
        "#" -> :rock
        "S" -> :start
      end
    end

    def fetch(%__MODULE__{} = grid, {x, y} = coords) when is_integer(x) and is_integer(y) do
      coords =
        if grid.infinite do
          translate(grid, coords)
        else
          coords
        end

      Map.fetch(grid.grid, coords)
    end

    defp translate(%__MODULE__{} = grid, {x, y}) do
      [x, y]
      |> Enum.map(fn coord ->
        coord
        # Truncate
        |> rem(grid.size)
        # Ensure positive
        |> add(grid.size)
        # Truncate again
        |> rem(grid.size)
      end)
      |> List.to_tuple()
    end

    defp add(a, b), do: a + b
  end

  defmodule Spread do
    def dirs({x, y}) do
      [{x, y + 1}, {x + 1, y}, {x, y - 1}, {x - 1, y}]
    end

    def spread(_, perimeter, _, 0, {_, that}) do
      {perimeter, that + map_size(perimeter)}
    end

    def spread(
          %Grid{} = grid,
          %{} = perimeter,
          %{} = last_perimeter,
          remaining_steps,
          {a_this, a_previous}
        ) do
      new_perimeter =
        perimeter
        |> Enum.map(fn {coords, _} ->
          dirs(coords)
          |> Enum.filter(fn coords -> Grid.fetch(grid, coords) == {:ok, :garden} end)
          |> Enum.filter(fn coords -> Map.fetch(last_perimeter, coords) == :error end)
          |> Enum.map(fn coords -> {coords, remaining_steps} end)
        end)
        |> List.flatten()
        |> Map.new()

      spread(
        grid,
        new_perimeter,
        perimeter,
        remaining_steps - 1,
        {a_previous + map_size(perimeter), a_this}
      )
    end
  end

  def solve() do
    IO.puts("Day 21")
    input = read_input()
    IO.puts("Part 1: #{part1(input, 64)}")
    IO.puts("Part 2: #{part2(input, 26_501_365)}")
  end

  def read_input() do
    File.read!("input/d21.txt")
  end

  def part1(input, steps) do
    {start, grid} =
      input
      |> Grid.parse()

    calculate(grid, start, steps)
  end

  def saturate(grid, start, iteration \\ 0, {twice_back, once_back} \\ {0, 0}) do
    this = calculate(grid, start, iteration)

    if twice_back == this do
      {once_back, this, iteration}
    else
      saturate(grid, start, iteration + 1, {once_back, this})
    end
  end

  def calculate(grid, start, steps) do
    start_perimeter = %{start => steps}
    {_, result} = Spread.spread(grid, start_perimeter, %{}, steps, {0, 0})

    result
  end

  def part2_brute(input, steps) do
    {start, grid} =
      input
      |> Grid.parse()

    grid = Grid.make_infinite(grid)

    calculate(grid, start, steps)
  end

  def part2(input, steps) do
    {start, grid} =
      input
      |> Grid.parse()

    {center_x, center_y} = start

    # Calculate the number of destinations in a saturated
    # grid (for odd and even iterations) (see tests)

    {even, odd, time_to_saturation} = saturate(grid, start)

    if steps < time_to_saturation do
      throw("Not enough steps for this method")
    end

    # Knowing that the grid is traversed horizontally and
    # vertically in constant speeds (see tests),
    # we can calculate:
    # a) the number of grid level iterations
    # b) the number of fully saturated grids

    full_iteration = grid.size
    half_iteration = div(grid.size, 2)

    iterations = div(steps - half_iteration, full_iteration)
    saturated = iterations - 1

    pair = fn o, e -> {(2 * o + 1) * (2 * o + 1), 4 * e * e} end
    {odd_blocks, even_blocks} = pair.(floor(saturated / 2), floor((saturated + 1) / 2))

    total_odd = odd_blocks * odd
    total_even = even_blocks * even

    # Add the remaining grids (they are not fully filled, so we need to calculate them)

    [
      total_even,
      total_odd,
      # 4 corners (all odd)
      calculate(grid, {center_x, grid.size - 1}, full_iteration + 1),
      calculate(grid, {center_x, 0}, full_iteration + 1),
      calculate(grid, {0, center_y}, full_iteration + 1),
      calculate(grid, {grid.size - 1, center_y}, full_iteration + 1),
      # N-1 tiles that are exposed for 65 iterations from a corner (they are "even")
      (iterations - 1) * calculate(grid, {grid.size - 1, grid.size - 1}, half_iteration),
      (iterations - 1) * calculate(grid, {0, grid.size - 1}, half_iteration),
      (iterations - 1) * calculate(grid, {grid.size - 1, 0}, half_iteration),
      (iterations - 1) * calculate(grid, {0, 0}, half_iteration),
      # N-2 tiles that are exposed for 131 iterations from a corner (they are "odd")
      max(iterations - 2, 0) * calculate(grid, {grid.size - 1, grid.size - 1}, full_iteration),
      max(iterations - 2, 0) * calculate(grid, {0, grid.size - 1}, full_iteration),
      max(iterations - 2, 0) * calculate(grid, {grid.size - 1, 0}, full_iteration),
      max(iterations - 2, 0) * calculate(grid, {0, 0}, full_iteration)
    ]
    |> IO.inspect()
    |> Enum.sum()
  end
end
