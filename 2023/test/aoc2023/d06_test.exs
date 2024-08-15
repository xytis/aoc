defmodule AOC2023.D06Test do
  use ExUnit.Case
  doctest AOC2023.D06

  @test_p1 <<"""
             Time:      7  15   30
             Distance:  9  40  200
             """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D06.part1(@test_p1) == 288
  end

  test "part2 on test data" do
    assert AOC2023.D06.part2(@test_p2) == 71503
  end

  test "internals" do
    assert AOC2023.D06.parse1(@test_p1) == [{7, 9}, {15, 40}, {30, 200}]
    assert AOC2023.D06.parse2(@test_p1) == {71530, 940_200}
  end
end
