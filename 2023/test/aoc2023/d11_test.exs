defmodule AOC2023.D11Test do
  use ExUnit.Case
  doctest AOC2023.D11

  @test_p1 <<"""
             ...#......
             .......#..
             #.........
             ..........
             ......#...
             .#........
             .........#
             ..........
             .......#..
             #...#.....
             """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D11.part1(@test_p1) == 374
  end

  test "part2 on test data" do
    assert AOC2023.D11.part2(@test_p2) == 82_000_210
  end

  test "internals" do
    assert AOC2023.D11.parse(@test_p1) == [
             {3, 0},
             {7, 1},
             {0, 2},
             {6, 4},
             {1, 5},
             {9, 6},
             {7, 8},
             {0, 9},
             {4, 9}
           ]

    assert AOC2023.D11.find_empty_space([{0, 0}, {2, 3}]) == {MapSet.new([1, 2]), MapSet.new([1])}

    assert AOC2023.D11.expand_space([{0, 0}, {2, 2}], {MapSet.new([1]), MapSet.new([1])}) == [
             {0, 0},
             {3, 3}
           ]

    assert AOC2023.D11.distance({1, 6}, {5, 11}) == 9
    assert AOC2023.D11.distance({1, 1}, {1, 1}) == 0
  end
end
