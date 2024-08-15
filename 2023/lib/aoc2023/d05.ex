defmodule AOC2023.D05 do
  def solve() do
    IO.puts("Day 5")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d05.txt")
  end

  def parse(input) do
    [
      seeds,
      seed_to_soil,
      soil_to_fertilizer,
      fertilizer_to_water,
      water_to_light,
      light_to_temperature,
      temperature_to_humidity,
      humidity_to_location
    ] = String.split(input, "\n\n", trim: true)

    converters =
      [
        parse_converter(seed_to_soil),
        parse_converter(soil_to_fertilizer),
        parse_converter(fertilizer_to_water),
        parse_converter(water_to_light),
        parse_converter(light_to_temperature),
        parse_converter(temperature_to_humidity),
        parse_converter(humidity_to_location)
      ]
      |> Enum.map(&build_converter/1)

    {
      seeds,
      fn input ->
        converters
        |> Enum.reduce(input, fn converter, p -> converter.(p) end)
      end
    }
  end

  def parse_seeds_1(input) do
    ["seeds", numbers] = String.split(input, ":", trim: true, parts: 2)

    String.split(numbers, " ", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.map(fn index -> index..index end)
  end

  def parse_seeds_2(input) do
    ["seeds", numbers] = String.split(input, ":", trim: true, parts: 2)

    String.split(numbers, " ", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(2, 2, :discard)
    |> Enum.map(fn [first, length] -> first..(first + length - 1) end)
  end

  def parse_converter(input) do
    [_header | overrides] = String.split(input, "\n", trim: true)

    Enum.map(overrides, fn line ->
      [destination, source, length] =
        String.split(line, " ", trim: true, parts: 3)
        |> Enum.map(&String.to_integer/1)

      {destination, source, length}
    end)
  end

  def fill_identity([]), do: []
  def fill_identity([last | []]), do: [last]

  def fill_identity([{_..al, _} = first | [{bf.._, _} = second | rest]]) do
    cond do
      # Consecutive ranges
      al + 1 == bf ->
        [first | fill_identity([second | rest])]

      # Non-consecutive ranges
      true ->
        [first | [{(al + 1)..(bf - 1), &Function.identity/1} | fill_identity([second | rest])]]
    end
  end

  def build_converter(overrides) do
    # rules are fully covering range from first rule start to last rule end
    rules =
      overrides
      |> Enum.sort(fn {_, a, _}, {_, b, _} -> a < b end)
      |> Enum.map(fn {destination, source, length} ->
        {source..(source + length - 1),
         fn first..last -> (first + (destination - source))..(last + (destination - source)) end}
      end)
      |> fill_identity()

    # find edges of rules
    {low.._, _} = List.first(rules)
    {_..high, _} = List.last(rules)

    fn ranges ->
      ranges
      |> Enum.map(fn rf..rl = range ->
        # check for range lower boundary
        first =
          if rf < low do
            if rl < low do
              [rf..rl]
            else
              [rf..(low - 1)]
            end
          else
            []
          end

        # check all the rules
        middle =
          rules
          |> Enum.map(fn {r, c} ->
            case range_intersection(range, r) do
              nil -> nil
              intersection -> c.(intersection)
            end
          end)
          |> Enum.filter(&(&1 != nil))

        # check for range upper boundary
        last =
          if rl > high do
            if rf > high do
              [rf..rl]
            else
              [(high + 1)..rl]
            end
          else
            []
          end

        List.flatten(first ++ middle ++ last)
      end)
      |> List.flatten()
    end
  end

  def range_intersection(%Range{} = a, %Range{} = b) do
    if a.first > b.last or b.first > a.last do
      nil
    else
      max(a.first, b.first)..min(a.last, b.last)
    end
  end

  def part1(input) do
    {seeds, converter} = parse(input)

    seeds = parse_seeds_1(seeds)

    converter.(seeds)
    |> Enum.map(fn f.._ -> f end)
    |> Enum.min()
  end

  def part2(input) do
    {seeds, converter} = parse(input)

    seeds = parse_seeds_2(seeds)

    converter.(seeds)
    |> Enum.map(fn f.._ -> f end)
    |> Enum.min()
  end
end
