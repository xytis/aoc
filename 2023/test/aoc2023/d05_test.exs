defmodule AOC2023.D05Test do
  use ExUnit.Case
  doctest AOC2023.D05

  @test_p1 <<"""
             seeds: 79 14 55 13

             seed-to-soil map:
             50 98 2
             52 50 48

             soil-to-fertilizer map:
             0 15 37
             37 52 2
             39 0 15

             fertilizer-to-water map:
             49 53 8
             0 11 42
             42 0 7
             57 7 4

             water-to-light map:
             88 18 7
             18 25 70

             light-to-temperature map:
             45 77 23
             81 45 19
             68 64 13

             temperature-to-humidity map:
             0 69 1
             1 0 69

             humidity-to-location map:
             60 56 37
             56 93 4
             """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D05.part1(@test_p1) == 35
  end

  test "part2 on test data" do
    assert AOC2023.D05.part2(@test_p2) == 46
  end

  test "internals" do
    converter = AOC2023.D05.build_converter([{50, 98, 2}, {52, 50, 48}])
    assert converter.([79..79]) == [81..81]
    assert converter.([14..14]) == [14..14]
    assert converter.([55..55]) == [57..57]
    assert converter.([13..13]) == [13..13]

    assert AOC2023.D05.parse_seeds_1("seeds: 79 14 55 13") == [79..79, 14..14, 55..55, 13..13]

    assert AOC2023.D05.parse_seeds_2("seeds: 79 14 55 13") == [
             79..(79 + 14 - 1),
             55..(55 + 13 - 1)
           ]

    assert AOC2023.D05.parse_converter("""
           soil-to-fertilizer map:
           0 15 37
           37 52 2
           39 0 15
           """) == [{0, 15, 37}, {37, 52, 2}, {39, 0, 15}]
  end
end
