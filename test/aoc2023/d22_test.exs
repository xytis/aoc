defmodule AOC2023.D22Test do
  use ExUnit.Case
  doctest AOC2023.D22

  @test_p1 <<"""
             """>>

  @test_p2 <<"""
             """>>

  test "part1 on test data" do
    assert AOC2023.D22.part1(@test_p1) == 0
  end

  test "part2 on test data" do
    assert AOC2023.D22.part2(@test_p2) == 0
  end

  test "internals" do
  end
end
