defmodule AOC2023.D09Test do
  use ExUnit.Case
  doctest AOC2023.D09

  @test_p1 <<"""
             0 3 6 9 12 15
             1 3 6 10 15 21
             10 13 16 21 30 45
             """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D09.part1(@test_p1) == 114
  end

  test "part2 on test data" do
    assert AOC2023.D09.part2(@test_p2) == 2
  end

  test "internals" do
    assert AOC2023.D09.parse(@test_p1) == [
             [0, 3, 6, 9, 12, 15],
             [1, 3, 6, 10, 15, 21],
             [10, 13, 16, 21, 30, 45]
           ]

    assert AOC2023.D09.resolve([0, 3, 6, 9, 12, 15]) == [3, 3, 3, 3, 3]
    assert AOC2023.D09.resolve([3, 3, 3, 3, 3]) == [0, 0, 0, 0]
  end
end
